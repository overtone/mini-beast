mini-beast
==========

Digital clone of an analog synthesizer using Overtone and Quil

<img src="https://github.com/overtone/mini-beast/raw/master/doc/minibeast.png" alt="MiniBeast Screenshot" />

## Prerequisites

- You need a reasonably recent Java installation, MiniBeast has been tested with OpenJDK 17 (Termurin)
- On some platforms you will have to [install SuperCollider](https://supercollider.github.io/downloads.html). We generally recommend you do this if you have any trouble running MiniBeast. Overtone/MiniBeast can run with its own embedded SuperCollider, but it frequently causes issues.
- To run from source you need the Clojure CLI tools

## Running from JAR

Download the latest `.jar` file from the Github [Releases](https://github.com/overtone/mini-beast/releases) page.

Invocations:

```shell
java -jar minibeast-<version>.jar
java -jar minibeast-<version>.jar --help
java -jar minibeast-<version>.jar --sc-boot-external

scsynth -u 12345
java -jar minibeast-<version>.jar --sc-udp-port 12345
```

Help:

```
     -·~=:#{[   ···Overtone···MINIBEAST···   ]}#:=~·-

minibeast [options] [patch-a] [patch-b]

  -x, --sc-boot-external     Boot a separate SuperCollider server process, instead of starting an embedded server.
  -u, --sc-udp-port PORT     Connect to an external SuperCollider server over UDP at the given port, instead of starting an embedded server.
  -v, --verbose           0  Verbosity level
  -h, --help
```

## Running from source

You need the [Clojure CLI tools](https://clojure.org/guides/install_clojure) for this. You may also have to.

```
git clone https://github.com/overtone/mini-beast.git
cd mini-beast
clojure -M -m minibeast.core
```

The above advice about running an external SuperCollider still applies, you can use the same CLI flags (`--sc-boot-external`, `--sc-udp-port`).

## Tips

* Make sure your MIDI devices are connected before starting the beast
* Right click on a knob slider, selector, or wheel and adjust the control on your MIDI device.
* The beast remembers your control bindings, so once you've bound one control, bind another!
* You can save and load bindings and presets using the `File` menu.

## Troubleshooting

### It runs but there's no sound

Check that your outputs are connected. On Linux this is done by connecting
Overtone to your audio interface through Jack or PipeWire. Programs that can do
this are `qjackctl` (Jack or Pipewire) or `qpwgraph` (Pipewire only).

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
pw-jack java -jar minibeast.jar --sc-udp-port 12345
```

## Building

To build the jar:

```
clojure -X:uberjar :jar minibeast-0.0.0.jar
```

## A call for patches

MiniBeast loads a default patch, but we need your help! There is a world of
sounds and timbres waiting to be explored. When you happen upon an unusual or
interesting patch, please save it and share it. Pull requests work or post them
to the Overtone [mailing list](http://groups.google.com/group/overtone).

Enjoy making new sounds!


