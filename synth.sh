yosys -p "synth_ice40 -json brainstem.json" BrainStem.v
nextpnr-ice40 --json brainstem.json --freq 12 --up5k --package sg48 --pcf breaker.pcf --asc brainstem.asc --pcf-allow-unconstrained
icepack brainstem.asc brainstem.bin
