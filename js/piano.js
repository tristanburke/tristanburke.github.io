// Piano.js | Copyright (c) 2017 Tristan Burke | www.tristan-burke.com

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//Slider
$('#ex1').slider({
    formatter: function(value) {
        return 'Current value: ' + value;
    }
});
//Octave Button
var octave_number=0;

function loweroctave() {
    if (octave_number <= 0) {
        octave_number = 0;
    } else {
        octave_number -= 1;
    }
}
function raiseoctave() {
    if (octave_number >= 2) {
        octave_number = 2;
    } else {
        octave_number += 1;
    }
}
//initialize note hertz for three octaves
var chertz =[261.626];
var cshertz =[277.183];
var dhertz =[293.665];
var dshertz =[311.127];
var ehertz =[329.628];
var fhertz =[349.228];
var fshertz =[369.994];
var ghertz =[391.995];
var gshertz =[415.305];
var ahertz =[440.000];
var ashertz =[466.164];
var bhertz =[493.883];
var c1hertz =[523.251];

//Toggle Buttons for wave type
var wave = 'sine';
function sine(){
    wave = 'sine';
    document.getElementById("sine").style.backgroundColor="#d2d2d2";
    document.getElementById("sawtooth").style.backgroundColor="white";
    document.getElementById("triangle").style.backgroundColor="white";
}
function sawtooth(){
    wave = 'sawtooth';
    document.getElementById("sine").style.backgroundColor="white";
    document.getElementById("sawtooth").style.backgroundColor="#d2d2d2";
    document.getElementById("triangle").style.backgroundColor="white";
}
function triangle(){
    wave = 'triangle';
    document.getElementById("sine").style.backgroundColor="white";
    document.getElementById("sawtooth").style.backgroundColor="white";
    document.getElementById("triangle").style.backgroundColor="#d2d2d2";
}
sine();

//Set Listener
window.onload=function(){
    document.addEventListener("keydown",play);
}
function chColor(key){
    key.style.backgroundColor='#d2d2d2';
}
function chBackColor(key){
    key.style.backgroundColor='white';
}
function chColorBlackKey(key){
    key.style.backgroundColor='#424242';
}
function chBackColorBlackKey(key){
    key.style.backgroundColor='black';
}
var audioCtx = new(window.AudioContext || window.webkitAudioContext)();
function play() {
    switch(event.keyCode){
        case 65: // C
            sound(chertz[octave_number]);
            var key = document.getElementById('c');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 87: //C#
            sound(cshertz[octave_number]);
            var key = document.getElementById('cs');
            chColorBlackKey(key);
            setTimeout(function() { chBackColorBlackKey(key); }, 300);
            break;
        case 83: //D
            sound(dhertz[octave_number]);
            var key = document.getElementById('d');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 69: //D#
            sound(dshertz[octave_number]);
            var key = document.getElementById('ds');
            chColorBlackKey(key);
            setTimeout(function() { chBackColorBlackKey(key); }, 300);
            break;
        case 68: //E
            sound(ehertz[octave_number]);
            var key = document.getElementById('e');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 70: //F
            sound(fhertz[octave_number]);
            var key = document.getElementById('f');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 84: //F#
            sound(fshertz[octave_number]);
            var key = document.getElementById('fs');
            chColorBlackKey(key);
            setTimeout(function() { chBackColorBlackKey(key); }, 300);
            break;
        case 71: //G
            sound(ghertz[octave_number]);
            var key = document.getElementById('g');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 89: //G#
            sound(gshertz[octave_number]);
            var key = document.getElementById('gs');
            chColorBlackKey(key);
            setTimeout(function() { chBackColorBlackKey(key); }, 300);
            break;
        case 72: //A
            sound(ahertz[octave_number]);
            var key = document.getElementById('a');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 85: //A#
            sound(ashertz[octave_number]);
            var key = document.getElementById('as');
            chColorBlackKey(key);
            setTimeout(function() { chBackColorBlackKey(key); }, 300);
            break;
        case 74: //B
            sound(bhertz[octave_number]);
            var key = document.getElementById('b');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        case 75: //C
            sound(c1hertz[octave_number]);
            var key = document.getElementById('c1');
            chColor(key);
            setTimeout(function() { chBackColor(key); }, 300);
            break;
        default:
            break;
    }

}
function sound(freq) {
    var oscillator = audioCtx.createOscillator();
    var gainNode = audioCtx.createGain();

    oscillator.connect(gainNode);
    gainNode.connect(audioCtx.destination);

    gainNode.gain.value = $('#ex1').slider('getValue')/10;
    oscillator.frequency.value = freq;
    oscillator.type = wave;

    oscillator.start();
    gainNode.gain.setValueAtTime(gainNode.gain.value, audioCtx.currentTime);
    gainNode.gain.linearRampToValueAtTime($('#ex1').slider('getValue')/10, audioCtx.currentTime + 0.01);
    setTimeout(
        function() {
            gainNode.gain.setValueAtTime(gainNode.gain.value, audioCtx.currentTime);
            gainNode.gain.exponentialRampToValueAtTime(0.0001, audioCtx.currentTime + 0.03);
        },
        300
    );
}