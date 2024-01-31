---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil i Bash betyr å lagre tekstdata i en fil. Programmerere gjør dette for å automatisere og lagre output av skript, for konfigurasjoner eller loggføring.

## Hvordan:

Lag en enkel tekstfil:
```Bash
echo "Dette er en test" > minfil.txt
```

Legg til mer tekst:
```Bash
echo "Legger til mer tekst" >> minfil.txt
```

Vis innholdet i filen:
```Bash
cat minfil.txt
```

Sample output:
```
Dette er en test
Legger til mer tekst
```

## Dybde:
Historisk sett bruker Bash (og de fleste Unix-systemer) tekstfiler for konfigurering og logging - det er enkelt og universelt. Alternativer til `echo` inkluderer `printf` for mer komplisert formatert tekst, eller redigeringsverktøy som `sed` og `awk` for automatisering. Når du lagrer tekst, bruker Bash `>` for å overskrive og `>>` for å legge til tekst i en fil uten å overskrive.

## Se Også:
- Bash Manual: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Shell Scripting Tutorial: https://www.shellscript.sh/
