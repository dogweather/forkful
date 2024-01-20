---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debugging i Bash: En Hands-On Guide

## Hva & Hvorfor?

`printf` eller `echo` i Bash lar deg skrive ut debug-informasjon - dette er semantisk data som hjelper deg å forstå hva som skjer underveis i koden din. Og hvorfor vi bruker dette? Enkelt: å finne feil raskere og lettere! 

## Hvordan gjør man det:

La oss si at vi har en skript som gjør flere ting, men vi er ikke sikre på hva som skjedde etter at vi kjørte det. Se følgende kode:

```Bash
#!/bin/bash
navn="Ola Nordmann"
echo "Hei, mitt navn er $navn"
```
Når du kjører denne skripten, vil det skrive ut: `Hei, mitt navn er Ola Nordmann`

Vi kan legge til en debug-melding rett før `echo` kommandoen slik:

```Bash
#!/bin/bash
navn="Ola Nordmann"
echo "Debug: navn har nå verdien $navn"
echo "Hei, mitt navn er $navn"
```

Nå får vi litt mer info når vi kjører skriptet: `Debug: navn har nå verdien Ola Nordmann`

## Deep Dive

Historisk sett har `echo` vært standarden for å printe tekster til terminalen i Unix/Linux miljøer. Men siden `printf` funksjonen ble introdusert, har den sakte men sikkert tatt over. `printf` gir det mer kontroll og er mer portabel mellom ulike operativsystemer.

Det er også noe alternativ til `printf` og `echo` for debugging, som `set -x` som gir deg muligheten til å spore utgivelsen av hver kommando i skriptet ditt. Slik kan du se akkurat hvor ting begynner å gå galt.

Fra implementasjonsdetaljene, skal `printf` og `echo` skrive til standard output (`stdout`). Du kan imidlertid omdirigere denne utgangen til filer, pipes, osv. Du kan også sende inn tekst som skal skrives ut som parametere til enten `printf` eller `echo`.

## Se Også

For mer informasjon om debugging og logger i Bash, sjekk følgende lenker:
* [Advanced Bash-Scripting Guide - debugging](http://tldp.org/LDP/abs/html/debugging.html)
* [Bash man side](http://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html) - en detaljert manual for Bash, inkludert 'echo' og 'printf'