---
title:                "Sjekke om en mappe finnes"
html_title:           "Arduino: Sjekke om en mappe finnes"
simple_title:         "Sjekke om en mappe finnes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Å sjekke om en mappe eksisterer lar oss forberede koden på forskjellige scenarier. Programmerere gjør dette for å unngå feil når de jobber med filsystemet.

## How to:
```Bash
# Sjekke om en mappe eksisterer
if [ -d "/sti/til/mappen" ]; then
  echo "Mappen eksisterer."
else
  echo "Mappen finnes ikke."
fi
```
Sample output:
```
Mappen eksisterer.
```

## Deep Dive
Historisk sett er sjekking av mapper en del av skallskriptingens grunnarbeid. Alternativene inkluderer 'test'-kommandoen (test -d), og utvidet versjon '[[' som gir flere funksjoner (f.eks. regex). Detaljer som skjuler seg bak inkluderer skjulte filer (.mappe), symlinker og tilgangsrettigheter som påvirker resultatet av eksistensen.

## See Also
- Bash man page: https://www.gnu.org/software/bash/manual/bash.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Filesystem Hierarchy Standard: https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html
