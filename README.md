mini-beast
==========

Digital clone of an analog synthesizer using Overtone and Quil

<img src="https://github.com/overtone/mini-beast/raw/master/doc/minibeast.png" alt="MiniBeast Screenshot" />

## Prerequisites

Install the [Clojure CLI tools](https://clojure.org/guides/install_clojure)

## Running

```
git clone https://github.com/overtone/mini-beast.git
cd mini-beast
clojure -M -m minibeast.core
```

This will try to use the Overtone internal (embedded) SuperCollider. There are
several scenarios in which this may not work (Mac M1/M2, Windows 64 bit, Linux
with Pipewire). Check the [[Troubleshooting]] section below for how to fix that.

## Tips

* Make sure your MIDI devices are connected before starting the beast
* Right click on a knob slider, selector, or wheel and adjust the control on your MIDI device.
* The beast remembers your control bindings, so once you've bound one control, bind another!
* You can save and load bindings and presets using the `File` menu.

## Troubleshooting

### Can't start SuperCollider

If the embedded SuperCollider doesn't work for some reason, then [install
SuperCollider](https://supercollider.github.io/downloads.html) manually. It's
packaged for virtually every operating system, so check your package manager
first.

Debian/Ubuntu:

```
sudo apt-get install supercollider-server sc3-plugins
scsynth -v
```

Once `scsynth` is on your path you can try `--sc-boot-external`, or start
it yourself (`scsynth -u 12345`), then connect to it with `--sc-udp-port 12345`.

On Linux when using PipeWire it might help to run MiniBeast with `pw-jack`, part
of the `pipewire-jack` package.

```
scsynth -u 12345
pw-jack clojure -M -m minibeast-core --sc-udp-port 12345
```

### It runs but there's no sound

Check that your outputs are connected. On Linux this is done by connecting
Overtone to your audio interface through Jack or PipeWire. Programs that can do
this are `qjackctl` (Jack or Pipewire) or `qpwgraph` (Pipewire only).

## A call for patches

MiniBeast loads a default patch, but we need your help! There is a world of
sounds and timbres waiting to be explored. When you happen upon an unusual or
interesting patch, please save it and share it. Pull requests work or post them
to the Overtone [mailing list](http://groups.google.com/group/overtone).

Enjoy making new sounds!


