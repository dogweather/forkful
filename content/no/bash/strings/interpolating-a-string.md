---
date: 2024-01-20 17:50:17.560921-07:00
description: "Hvordan: Du kan ogs\xE5 bruke kr\xF8llparenteser for klarhet."
lastmod: '2024-04-05T21:53:41.921140-06:00'
model: gpt-4-1106-preview
summary: "Du kan ogs\xE5 bruke kr\xF8llparenteser for klarhet."
title: Interpolering av en streng
weight: 8
---

## Hvordan:
```Bash
name="Verden"
echo "Hei, $name!"
# Output: Hei, Verden!
```

Du kan også bruke krøllparenteser for klarhet:
```Bash
greeting="hei"
target="Verden"
echo "${greeting}, ${target}!"
# Output: hei, Verden!
```

Og for å manipulere verdien under interpoleringen:
```Bash
item="apple"
echo "I have ${item}s"
# Output: I have apples
```

## Dypdykk:
Strenginterpolering i kodespråk har vært rundt en stund. I eldre språk som Perl, ble dette populært på 80-tallet. Bash, arvingen til Sh shell, adopterte også denne funksjonen.

Alternativer til interpolering i Bash kan være å bruke `echo` med flere argumenter eller `printf` for mer formatert utskrift:
```Bash
echo "Hello" $name "!"
# Samme utskrift uten interpolering

printf "Hei, %s!\n" "$name"
# Mer kontroll over formatet
```

Implementeringsdetaljer? I Bash blir alt mellom dobbelte sitater utvidet eller `evaluated`. Det betyr at variabler, uttrykk, og kommandoer innenfor " " får kjørt sin funksjon.

## Se Også:
- Bash manualen: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Wiki om Shell Script: https://en.wikipedia.org/wiki/Shell_script

Disse kildene gir mer inngående kunnskap om Bash-skripting og dens funksjoner.
