---
date: 2024-01-20 17:47:06.833277-07:00
description: "\xC5 finne lengden p\xE5 en tekststreng betyr \xE5 telle antallet tegn\
  \ den inneholder. Vi gj\xF8r dette for \xE5 validere inndata, h\xE5ndtere tekstbaserte\
  \ protokoller,\u2026"
lastmod: '2024-03-13T22:44:40.963309-06:00'
model: gpt-4-1106-preview
summary: "\xC5 finne lengden p\xE5 en tekststreng betyr \xE5 telle antallet tegn den\
  \ inneholder. Vi gj\xF8r dette for \xE5 validere inndata, h\xE5ndtere tekstbaserte\
  \ protokoller,\u2026"
title: "Finn lengden p\xE5 en streng"
weight: 7
---

## Hva & Hvorfor?
Å finne lengden på en tekststreng betyr å telle antallet tegn den inneholder. Vi gjør dette for å validere inndata, håndtere tekstbaserte protokoller, eller for å formatere utdata riktig.

## Slik gjør du:
```Bash
streng="Hallo, Norge!"
lengde=${#streng}
echo "Lengden av strengen er: $lengde"
```

Utdata:
```
Lengden av strengen er: 13
```

## Dypdykk
Finner lengden på en streng i Bash er ganske rett frem og har vært en del av Bash helt siden tidlige versjoner. Historisk har ulike programmeringsspråk tilbudt forskjellige metoder for å måle strenglengder, som funksjoner eller metoder. I Bash bruker vi `${#variable}` for å få lengden, som er bygget direkte inn i Bash, og er derfor raskt og effektivt.

Alternativer inkluderer bruk av `expr` kommandoen eller `wc -m`, men disse kaller eksterne programmer og er ofte tregere:
```Bash
lengde=$(expr length "$streng")
echo "Lengden funnet med expr: $lengde"

lengde=$(echo -n "$streng" | wc -m)
echo "Lengden funnet med wc: $lengde"
```
Implementeringsdetaljer kan variere med ulike versjoner av Bash eller UNIX-lignende systemer, men `${#variable}` er den mest pålitelige og bærbare metoden på tvers av moderne systemer.

## Se også
- Bash manualen: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Bash-håndboken på norsk: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Index (detaljert, men ikke fullstendig oversatt)
