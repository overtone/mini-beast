mini-beast
==========

Digital clone of an analog synthesizer using Overtone and Quil

<img src="https://github.com/overtone/mini-beast/blob/master/doc/minibeast.png" alt="MiniBeast Screenshot" />

## Running
    git https://github.com/overtone/mini-beast.git
    cd mini-beast
    lein deps
then
    lein run
or
    lein repl
    (-main)

## Notes
Make sure your MIDI devices are connected before running lein run or lein repl.
After running mini-beast,  click on a knob slider, selector, or wheel and adjust the control on your MIDI device.
The synth control and MIDI control  are now bound to each other. Adjusting the control on your MIDI device
will now update the synth parameter.

## A call for patches
MiniBeast loads a default patch, but we need your help! There is a world of sounds and timbres waiting to be explored.
When you happen upon an unusual or interesting patch, please save it and share it. Pull requests work or post them to
the Overtone [mailing list](http://groups.google.com/group/overtone). 

Enjoy making new sounds!


