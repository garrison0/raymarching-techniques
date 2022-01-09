import * as twgl from './node_modules/twgl.js/dist/4.x/twgl-full.module.js';

const gl = document.querySelector("#c").getContext("webgl2");
var state = { mousePos: [0.5, 0.5] };

function init(preloaded, waterImage, grassImage, sandImage)
{
    state.programInfo = twgl.createProgramInfo(gl, [preloaded['vs'], preloaded['fs']]);

    state.bufferInfo = twgl.primitives.createXYQuadBufferInfo(gl);

    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    gl.clearColor(0.0,0.0,0.0,1.0);

    requestAnimationFrame(render);
}

function render(time) { 
    time *= 0.001; // milliseconds to seconds

    twgl.resizeCanvasToDisplaySize(gl.canvas, 1.0);
    gl.viewport(0, 0, gl.canvas.clientWidth, gl.canvas.clientHeight);

    const uniforms = {
        uTime: time,
        uResolution: [gl.canvas.clientWidth, gl.canvas.clientHeight]
    }

    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    gl.useProgram(state.programInfo.program);
    twgl.setUniforms(state.programInfo, uniforms);
    twgl.setBuffersAndAttributes(gl, state.programInfo, state.bufferInfo); 
    twgl.drawBufferInfo(gl, state.bufferInfo);

    requestAnimationFrame(render);
}


// -------- HANDLE BROWSER EVENTS / INTERACTIVITY             --------- //
document.onmousemove = (event) => { 
    const rect = gl.canvas.getBoundingClientRect();
    const x = (event.clientX - rect.left) / gl.canvas.width * 2 - 1;
    const y = (event.clientY - rect.top ) / gl.canvas.height * 2 - 1;
    state.mousePos = [2 * x,2 * -y];
}

// -------- FETCH FILES ( SHADERS, IMAGES FOR TEXTURES, ETC ) --------- //
// keys correspond to the file names of each shader
var files = {'fs': '', 'vs': ''};
var count = 0;
let numFiles = 2;

for (const key of Object.keys(files)) { 
  fetch('./shader/' + key + '.glsl')
    .then(response => response.text())
    .then(text => {
      files[key] = text;
      count = count + 1;
      if (count >= numFiles) 
        init(files);
    });
};
