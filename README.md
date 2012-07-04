mini-beast
==========

Digital clone of an analog synthesizer using Overtone and Quil

<img src="https://github.com/aaron-santos/mini-beast/raw/master/doc/minibeast.png" alt="Minibeast Screenshot" />

## Running
git clone https://github.com/aaron-santos/mini-beast
cd mini-beast
lein deps
lein repl

## Notes
Make sure your MIDI devices are connected before running lein repl.
After running lein repl,  click on a knob slider, selector, or wheel and adjust the control on your MIDI device.
The synth control and MIDI control  are now bound to each other. Adjusting the control on your MIDI device
will now update the synth parameter.

## Creating a patch
Start by moving the saw oscillator slider to the top of its range. Do the same to the sustain slider
in the filter and amplification envelope sections. Then turn up the master volume half wave and play
some notes using a MIDI keyboard.

Enjoy making new sounds!


