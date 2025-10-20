// === TELAS INFINITAS 3D + PAD CONTINUO SIN SILENCIOS ===
// p5.js + p5.sound
// 3 voces principales + 4ª voz de color. Armonía I–iii–vi–ii–V–I con transiciones no fijas.

// ==== PARÁMETROS VISUALES ====
let PLANE_SIZE = 3000;
let GRID_STEPS = 30;
let DIST_BETWEEN = 380;
let ALPHA_MIN = 8, ALPHA_MAX = 90, ALPHA_CURVE = 0.9;
let WAVE_SPEED = 0.18, NOISE_SCALE = 0.0017, NOISE_OCTAVES = 4, NOISE_GAIN = 0.5, ANISO_FACTOR = 1.6;
let WIND_DRIFT = 0.35, GUST_INTENS = 0.75, GUST_SPEED = 0.05;
let TRANSLATION_SPEED = 0.0, TRANSLATION_RADIUS = 120;
let PLANE_NORMAL, WIND_DIR_INIT, windDir, seedBase;

// Telas siempre más grandes que la pantalla
const PLANE_SCALE = 2.4; // 1.2–2.0 según preferencia
function updatePlaneSize(){ PLANE_SIZE = Math.max(windowWidth, windowHeight) * PLANE_SCALE; }

// ==== LUZ GLOBAL ====
let BRIGHTNESS_GAIN = 1.7;   // escala RGB
let ALPHA_GAIN      = 1.9;   // escala de opacidad
let ALPHA_CLAMP_MAX = 160;   // tope alfa
const WHITE_VEIL    = 80;    // 0..80 (0 = off)

// ==== SALIDA / MASTER ====
let preGain, postGain, limiter; // cadena: MIX -> preGain -> limiter -> postGain -> master
let OUT_VOL = 1;            // 0..1 control de volumen
let LIMITER_DRIVE = 5.4;       // 1..4 empuja al limitador (perceived loudness)
let LIMITER_ON = true;         // on/off lógico del limitador

function applyOutputGains(){
  // Ganancia previa al limitador (drive) y salida final
  preGain.amp(constrain(OUT_VOL * (LIMITER_ON ? LIMITER_DRIVE : 1.0), 0, 4.0));
  postGain.amp(1.0); // mantenemos unity en la salida
}

// ==== PARÁMETROS DE SÍNTESIS ====
const synthParams = {
  oscType: 'triangle',
  baseNote: 48, // C3
  scaleSteps: [0, 2, 4, 5, 7, 9, 11],
  crossfadeTime: 1.2,
  minRest: 10.0,
  changeInterval: [6, 15],
  amp: 1,
  delayTime: 0.900,
  delayFeedback: 0.25,
  delayFilter: 4000,
  reverbTime: 2.98,
  reverbDecay: 0.4
};

// ==== ARMÓNÍA ====
const CHORDS = [
  { name:'I',   tones:[0,4,7],     add:[2,9]  },
  { name:'iii', tones:[4,7,11],    add:[2,9]  },
  { name:'vi',  tones:[9,0,4],     add:[2,7]  },
  { name:'ii',  tones:[2,5,9],     add:[11,4] },
  { name:'V',   tones:[7,11,2],    add:[9]    }
];
const CHORD_GRAPH = {
  'I'  : [{to:'iii',w:2},{to:'vi',w:3},{to:'ii',w:2},{to:'V',w:3}],
  'iii': [{to:'vi',w:3},{to:'ii',w:2},{to:'V',w:2},{to:'I',w:2}],
  'vi' : [{to:'ii',w:3},{to:'V',w:3},{to:'I',w:2},{to:'iii',w:2}],
  'ii' : [{to:'V',w:4},{to:'I',w:2},{to:'vi',w:2}],
  'V'  : [{to:'I',w:5},{to:'vi',w:2},{to:'iii',w:1},{to:'ii',w:1}]
};
let chordIndex = 0;
const CHORD_CHANGE_PROB = 0.35;
const COLOR_PROB = 0.18;
const FOURTH_IN_CHORD_PROB = 0.7;

// ==== AUDIO ====
let audioStarted = false;
let osc = [], gain = [], delay = [], reverb;
let activeNotes = [];
let nextChangeTimes = [];
let changing = false;

// ==== RANGOS POR VOZ ====
const RANGES = [
  { low: 46, high: 54 },
  { low: 52, high: 60 },
  { low: 58, high: 66 },
  { low: 62, high: 76 }
];

// ==== CÁMARA (FIJA) ====
let cam;

// ==== SETUP ====
function setup() {
  createCanvas(windowWidth, windowHeight, WEBGL);
  noStroke();
  updatePlaneSize(); // telas > pantalla

  seedBase = random(10000);
  PLANE_NORMAL = createVector(0, 0, 1);
  WIND_DIR_INIT = createVector(1, 0.15, 0).normalize();
  windDir = WIND_DIR_INIT.copy();

  // Cámara fija (no se mueve nunca)
  cam = createCamera();
  cam.setPosition(0, 0, 0); // ajustá Z si querés más/menos zoom
  cam.lookAt(0, 0, 0);

  tryAutostart();
}

// NO usamos orbitControl(), así el click/drag nunca mueve la cámara.
// Igualmente, dejamos los gestos para iniciar audio si hace falta.
function mousePressed(){ startAudioIfNeeded(); }
function touchStarted(){ startAudioIfNeeded(); }
function keyPressed(){ startAudioIfNeeded(); }

async function tryAutostart(){
  try{
    await getAudioContext().resume();
    initAudio();
    audioStarted = true;
  }catch(e){}
}
function startAudioIfNeeded(){
  if(audioStarted) return;
  userStartAudio();
  initAudio();
  audioStarted = true;
}

function initAudio(){
  // Buses / cadena master
  preGain  = new p5.Gain();
  postGain = new p5.Gain();
  limiter  = new p5.Compressor();

  // Configuración tipo "brickwall limiter"
  // threshold bajo, ratio muy alto, ataque corto, release corto, knee duro
  limiter.threshold(-3);  // dB
  limiter.ratio(20);      // ~brickwall
  limiter.attack(0.003);  // s
  limiter.release(0.12);  // s
  limiter.knee(0);        // dB

  // Efectos
  reverb = new p5.Reverb();

  // Voces
  for(let i=0;i<4;i++){
    osc[i]   = new p5.Oscillator(synthParams.oscType);
    gain[i]  = new p5.Gain();
    delay[i] = new p5.Delay();

    osc[i].disconnect();
    osc[i].connect(gain[i]);
    gain[i].connect(delay[i]);
    delay[i].connect(reverb);

    const amp = (i===3) ? synthParams.amp*0.45 : synthParams.amp;
    gain[i].amp(amp);

    delay[i].process(
      gain[i],
      synthParams.delayTime + random(-0.1,0.1),
      synthParams.delayFeedback,
      synthParams.delayFilter
    );
    reverb.process(delay[i], synthParams.reverbTime, synthParams.reverbDecay);
  }

  // Ruta final: REVERB -> preGain -> LIMITER -> postGain -> master
  reverb.disconnect();
  reverb.connect(preGain);
  preGain.connect(limiter);
  limiter.connect(postGain);
  postGain.connect(); // a master

  applyOutputGains();

  // Notas iniciales
  for(let i=0;i<4;i++){
    let n = pickNoteHarmonic(activeNotes, i);
    activeNotes[i]=n;
    osc[i].freq(midiToFreq(n));
    osc[i].start();

    const jitter = (i===3)? 1.5 : 1.0;
    nextChangeTimes[i] = seconds() + random(synthParams.changeInterval[0], synthParams.changeInterval[1])*jitter;
  }
}

// ==== DRAW ====
function draw(){
  background(15);

  // Overlay de inicio
  if(!audioStarted){
    push();
    resetMatrix();
    translate(-width/2, -height/2, 0);
    noStroke(); fill(0,220); rect(0,0,width,height);
    fill(255); textAlign(CENTER,CENTER); textSize(18);
    text('clic / tap / tecla para iniciar el audio', width/2, height/2);
    pop();
  }

  // Cámara no se mueve (no orbitControl), la fijamos por si acaso
  cam.setPosition(0, 0, 1400);
  cam.lookAt(0, 0, 0);

  let t = seconds();
  updateWind(t);

  // Movimiento global sutil de las telas (no de la cámara)
  let globalOffset = createVector(
    sin(t*TRANSLATION_SPEED)*TRANSLATION_RADIUS,
    cos(t*TRANSLATION_SPEED*0.7)*TRANSLATION_RADIUS,
    sin(t*TRANSLATION_SPEED*0.5)*TRANSLATION_RADIUS
  );

  blendMode(ADD);

  let colors = [color(255,0,0), color(0,255,0), color(0,0,255)];
  for(let i=0;i<3;i++){
    let offset = (i-1)*DIST_BETWEEN;
    push(); translate(globalOffset.x,globalOffset.y,globalOffset.z);
    drawPlane(PLANE_NORMAL, offset, colors[i], t, i);
    pop();
  }

  blendMode(BLEND);

  if (WHITE_VEIL > 0) {
    push();
    resetMatrix(); translate(-width/2, -height/2, 0);
    noStroke(); fill(255, WHITE_VEIL); rect(0,0,width,height);
    pop();
  }

  if(audioStarted) evolveHarmony(t);
}

// ==== ARMONÍA / CAMBIOS DE VOZ ====
function evolveHarmony(t){
  if(changing) return;

  for(let i=0;i<4;i++){
    if(t > nextChangeTimes[i]){
      changing = true;

      if(i!==3 && random()<CHORD_CHANGE_PROB){
        chordIndex = chooseNextChordIndex(chordIndex);
      }

      let currentNotes = activeNotes.slice();
      let newNote = (i===3) ? pickNoteColor(currentNotes, i)
                            : pickNoteHarmonic(currentNotes, i);

      const oldF = osc[i].getFreq();
      const newF = midiToFreq(newNote);
      const startT = seconds();

      let fade = setInterval(()=>{
        let k = constrain((seconds()-startT)/synthParams.crossfadeTime, 0, 1);
        osc[i].freq( lerp(oldF, newF, easeInOut(k)) );
        if(k>=1){
          clearInterval(fade);
          activeNotes[i] = newNote;

          const jitter = (i===3)? 1.5 : 1.0;
          nextChangeTimes[i] = seconds()
                              + random(synthParams.changeInterval[0], synthParams.changeInterval[1])*jitter
                              + synthParams.minRest;
          setTimeout(()=>changing=false, synthParams.minRest*1000);
        }
      }, 30);

      break;
    }
  }
}

// ==== ELECCIÓN DE NOTAS ====
function pickNoteHarmonic(exclude, voiceIndex){
  const R = RANGES[voiceIndex];
  const chord = CHORDS[chordIndex];
  let toneSet = chord.tones.slice();
  if(random() < COLOR_PROB && chord.add && chord.add.length){
    toneSet = toneSet.concat( random(chord.add) );
  }
  const candidates = [];
  for(const semi of toneSet){
    const base = R.low - (R.low % 12);
    for(let o=-2; o<=4; o++){
      const n = base + 12*o + semi;
      if(n>=R.low && n<=R.high) candidates.push(n);
    }
  }
  if(candidates.length===0) return fallbackInRange(R);

  const current = activeNotes[voiceIndex] ?? (R.low+R.high>>1);
  candidates.sort((a,b)=>Math.abs(a-current)-Math.abs(b-current));

  for(const note of candidates){
    if(exclude.includes(note)) continue;
    if(!clashesSecond(note, exclude)) return note;
  }
  return candidates[0];
}

function pickNoteColor(exclude, voiceIndex){
  const R = RANGES[voiceIndex];
  const chord = CHORDS[chordIndex];
  const scaleSet = synthParams.scaleSteps.slice();

  let allowedSemis;
  if(random() < FOURTH_IN_CHORD_PROB){
    allowedSemis = chord.tones.slice();
  }else{
    allowedSemis = scaleSet.filter(s=>!chord.tones.includes(s));
    if(allowedSemis.length===0) allowedSemis = scaleSet;
  }

  const candidates = [];
  for(const semi of allowedSemis){
    const base = R.low - (R.low % 12);
    for(let o=-2;o<=4;o++){
      const n = base + 12*o + semi;
      if(n>=R.low && n<=R.high) candidates.push(n);
    }
  }
  if(candidates.length===0) return fallbackInRange(R);

  const current = activeNotes[voiceIndex] ?? (R.low+R.high>>1);
  candidates.sort((a,b)=>Math.abs(a-current)-Math.abs(b-current));

  for(const note of candidates){
    if(exclude.includes(note)) continue;
    if(!clashesSecond(note, exclude)) return note;
  }
  return candidates[0];
}

function clashesSecond(note, arr){
  return arr.some(e=>{
    let d = Math.abs((note - e) % 12);
    if(d>6) d = 12 - d;
    return d===1 || d===2;
  });
}
function fallbackInRange(R){
  const mid = (R.low+R.high)>>1;
  const semi = mid%12;
  let best = synthParams.scaleSteps[0], bestD=99;
  for(const s of synthParams.scaleSteps){
    const d = Math.abs(s-semi); if(d<bestD){ bestD=d; best=s; }
  }
  return mid - (mid%12) + best;
}

// ==== TRANSICIÓN DE ACORDES ====
function chooseNextChordIndex(currentIndex){
  const current = CHORDS[currentIndex].name;
  const edges = CHORD_GRAPH[current] || [];
  if(edges.length===0){
    let idx; do { idx = int(random(CHORDS.length)); } while(idx===currentIndex);
    return idx;
  }
  let sum = edges.reduce((a,e)=>a+e.w,0);
  let r = random(sum), acc=0;
  for(const e of edges){
    acc += e.w;
    if(r <= acc){
      const idx = CHORDS.findIndex(c=>c.name===e.to);
      return (idx>=0 && idx!==currentIndex) ? idx : currentIndex;
    }
  }
  return currentIndex;
}

// ==== UTIL MUSICALES / TIEMPO ====
function easeInOut(x){ return 0.5 - 0.5 * cos(PI*x); }
function seconds(){ return millis()/1000; }

// ==== VISUAL ====
function fbm(x,y,z){
  let amp=1,sum=0,sc=1;
  for(let i=0;i<NOISE_OCTAVES;i++){ sum+=amp*noise(x*sc,y*sc,z*sc); amp*=NOISE_GAIN; sc*=2; }
  return sum;
}
function alphaAt(pos,t,layerId){
  let tangW = projectOntoPlane(windDir, PLANE_NORMAL).normalize();
  let u = pos.dot(tangW);
  let v = pos.dot(anyPerp(tangW, PLANE_NORMAL));
  let timeMain = t*WAVE_SPEED;
  let gust = GUST_INTENS*(noise(0.7*layerId + GUST_SPEED*t)-0.5)*2.0;
  let nx=(u*ANISO_FACTOR)*NOISE_SCALE;
  let ny=v*NOISE_SCALE;
  let nz=(timeMain+gust)+10.123*layerId;
  let n=constrain(fbm(nx+seedBase*0.137, ny+seedBase*0.271, nz), 0, 1);
  if(ALPHA_CURVE!==1.0) n=pow(n, ALPHA_CURVE);
  return lerp(ALPHA_MIN, ALPHA_MAX, n);
}
function drawPlane(normal,offset,col,t,layerId){
  let basis=basisFromNormal(normal);
  let S=PLANE_SIZE/2, du=(2*S)/GRID_STEPS, dv=(2*S)/GRID_STEPS;
  let origin=p5.Vector.mult(normal,offset);
  noStroke();

  // RGB con ganancia de brillo
  let r = Math.min(255, red(col)   * BRIGHTNESS_GAIN);
  let g = Math.min(255, green(col) * BRIGHTNESS_GAIN);
  let b = Math.min(255, blue(col)  * BRIGHTNESS_GAIN);

  beginShape(TRIANGLES);
  for(let iy=-GRID_STEPS/2; iy<GRID_STEPS/2; iy++){
    for(let ix=-GRID_STEPS/2; ix<GRID_STEPS/2; ix++){
      let p0=planePoint(origin,basis,ix*du,iy*dv);
      let p1=planePoint(origin,basis,(ix+1)*du,iy*dv);
      let p2=planePoint(origin,basis,ix*du,(iy+1)*dv);
      let p3=planePoint(origin,basis,(ix+1)*du,(iy+1)*dv);

      let a0=alphaAt(p0,t,layerId), a1=alphaAt(p1,t,layerId),
          a2=alphaAt(p2,t,layerId), a3=alphaAt(p3,t,layerId);

      // Alfa con ganancia y tope
      a0 = constrain(a0 * ALPHA_GAIN, 0, ALPHA_CLAMP_MAX);
      a1 = constrain(a1 * ALPHA_GAIN, 0, ALPHA_CLAMP_MAX);
      a2 = constrain(a2 * ALPHA_GAIN, 0, ALPHA_CLAMP_MAX);
      a3 = constrain(a3 * ALPHA_GAIN, 0, ALPHA_CLAMP_MAX);

      fill(r,g,b,a0); vertex(p0.x,p0.y,p0.z);
      fill(r,g,b,a1); vertex(p1.x,p1.y,p1.z);
      fill(r,g,b,a2); vertex(p2.x,p2.y,p2.z);

      fill(r,g,b,a1); vertex(p1.x,p1.y,p1.z);
      fill(r,g,b,a3); vertex(p3.x,p3.y,p3.z);
      fill(r,g,b,a2); vertex(p2.x,p2.y,p2.z);
    }
  }
  endShape();
}

// ==== UTILIDADES VECTORES ====
function planePoint(origin,b,u,v){
  return p5.Vector.add(p5.Vector.add(origin,p5.Vector.mult(b.U,u)),p5.Vector.mult(b.V,v));
}
function basisFromNormal(n){
  let helper = Math.abs(n.z)<0.9 ? createVector(0,0,1) : createVector(0,1,0);
  let U=n.copy().cross(helper).normalize();
  let V=n.copy().cross(U).normalize();
  return {U,V,N:n.copy()};
}
function projectOntoPlane(v,n){ return p5.Vector.sub(v, p5.Vector.mult(n, v.dot(n))); }
function anyPerp(a,n){ return n.copy().cross(a).normalize(); }
function updateWind(t){
  let k=0.35;
  let nx=noise(5.12+k*t)-0.5, ny=noise(31.77+k*t)-0.5, nz=noise(73.91+k*t)-0.5;
  let drift=createVector(nx, ny*0.5, nz).mult(WIND_DRIFT);
  windDir=p5.Vector.add(WIND_DIR_INIT, drift);
  let tang=projectOntoPlane(windDir, PLANE_NORMAL);
  if(tang.mag()<1e-3) tang=anyPerp(PLANE_NORMAL, createVector(1,0,0));
  windDir=tang.normalize();
}
function windowResized(){
  resizeCanvas(windowWidth, windowHeight);
  updatePlaneSize(); // mantener telas > pantalla
}
