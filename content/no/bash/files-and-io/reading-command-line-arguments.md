---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:55:20.388578-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese kommandolinjeargumenter betyr ganske enkelt at skriptet ditt plukker opp informasjon direkte fra terminalen. Programmerere bruker dette til å gjøre skriptene fleksible og tilpassbare for ulike oppgaver uten å endre på koden hver gang.

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
