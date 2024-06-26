---
date: 2024-01-20 17:47:06.833277-07:00
description: "Slik gj\xF8r du: Finner lengden p\xE5 en streng i Bash er ganske rett\
  \ frem og har v\xE6rt en del av Bash helt siden tidlige versjoner. Historisk har\
  \ ulike\u2026"
lastmod: '2024-04-05T22:50:54.965439-06:00'
model: gpt-4-1106-preview
summary: "Finner lengden p\xE5 en streng i Bash er ganske rett frem og har v\xE6rt\
  \ en del av Bash helt siden tidlige versjoner."
title: "Finn lengden p\xE5 en streng"
weight: 7
---

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
