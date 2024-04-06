---
date: 2024-01-20 17:55:20.388578-07:00
description: "Hvordan: Kj\xF8r skriptet med `./hilsen.sh Verden`, og du f\xE5r."
lastmod: '2024-04-05T21:53:41.951681-06:00'
model: gpt-4-1106-preview
summary: "Kj\xF8r skriptet med `./hilsen.sh Verden`, og du f\xE5r."
title: Lese kommandolinjeargumenter
weight: 23
---

## Hvordan:
```Bash
#!/bin/bash
echo "Hei, $1!"
```
Kjør skriptet med `./hilsen.sh Verden`, og du får:
```
Hei, Verden!
```
For å håndtere flere argumenter:
```Bash
#!/bin/bash
echo "Hei, $1!"
echo "Velkommen til $2 kurs."
```
Kjør med `./hilsen.sh Ola Bash`, og resultatet er:
```
Hei, Ola!
Velkommen til Bash kurs.
```

## Dypdykk
Kommandolinjeargumenter ble introdusert tidlig i UNIX-historien for at skript og programmer skulle kunne håndtere ulike situasjoner uten endringer. `$1`, `$2`, og så videre representerer hvert sitt argument. `$0` er selve skriptnavnet.

Åpne filer eller prosesser med navn som argumenter er en vanlig praksis. Alternativer til `$1` inkluderer `getopts` for mere avanserte alternativer og `shift` for å traversere argumenter.

Når det gjelder implementasjonsdetaljer, husk på at ubrukte argumenter ignoreres. Sjekk alltid for gyldighet og antall argumenter for robusthet. Bruk `$#` til å få antall argumenter.

## Se Også
- [The Bash Guide](https://guide.bash.academy/)
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
