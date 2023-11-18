mini-beast
==========

Digital clone of an analog synthesizer using Overtone and Quil

<img src="https://github.com/overtone/mini-beast/raw/master/doc/minibeast.png" alt="MiniBeast Screenshot" />

## Prerequisites

Install the [Clojure CLI tools](https://clojure.org/guides/install_clojure)

## Running
    git clone https://github.com/overtone/mini-beast.git
    cd mini-beast
    clojure -M -m minibeast.core --help

## Tips
* Make sure your MIDI devices are connected before starting the beast
* Click on a knob slider, selector, or wheel and adjust the control on your MIDI device.
* The beast remembers your control bindings, so once you've bound one control, bind another!
* You can save and load bindings and presets using the `File` menu.

## Troubleshooting

If the embedded SuperCollider doesn't work for some reason, then install
SuperCollider manually (it's packaged for virtually any operating system). If
`scsynth` is on your path you can try `--sc-boot-external`, or start it yourself
(`scsynth -u 12345`), then connect to it with `--sc-udp-port 12345`.

On Linux when using PipeWire it might help to run MiniBeast with `pw-jack`

```
scsynth -u 12345
pw-jack clojure -M -m minibeast-core --sc-udp-port 12345
```

## A call for patches
MiniBeast loads a default patch, but we need your help! There is a world of sounds and timbres waiting to be explored.
When you happen upon an unusual or interesting patch, please save it and share it. Pull requests work or post them to
the Overtone [mailing list](http://groups.google.com/group/overtone). 

Enjoy making new sounds!


