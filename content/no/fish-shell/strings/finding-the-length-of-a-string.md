---
date: 2024-01-20 17:47:20.963008-07:00
description: "\xC5 finne lengden p\xE5 en streng betyr \xE5 telle antall tegn den\
  \ inneholder. Programmerere gj\xF8r dette for \xE5 validere inndata, skj\xE6re strenger\
  \ eller for \xE5\u2026"
lastmod: '2024-03-13T22:44:41.216066-06:00'
model: gpt-4-1106-preview
summary: "\xC5 finne lengden p\xE5 en streng betyr \xE5 telle antall tegn den inneholder.\
  \ Programmerere gj\xF8r dette for \xE5 validere inndata, skj\xE6re strenger eller\
  \ for \xE5\u2026"
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng betyr å telle antall tegn den inneholder. Programmerere gjør dette for å validere inndata, skjære strenger eller for å manipulere tekstbasert data effektivt.

## Slik gjør du:
I Fish Shell, bruk `string length` for å få lengden på en streng.

```Fish Shell
set min_streng "Heisann"
echo (string length "$min_streng")
```

Forventet output:

```
7
```

## Dypdykk:
Historisk sett hadde ikke tidlige Unix-skall innebygde funksjoner for enkel manipulasjon av strenger; dette krevde bruk av eksterne verktøy som `awk` eller `wc`. Fish Shell, et nyere og mer moderne skall, inkluderer brukervennlige innebygde kommandoer som `string length`. Det er alternativer som å bruke `wc -m`, men `string length` er mer leselig og direkte. Implementasjonen benytter en innebygd funksjon som effektivt itererer gjennom strengen for å telle tegn, noe som er mer optimalt enn å pipe data gjennom flere verktøy.

## Se Også:
- Fish Shell dokumentasjon for strenger: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Unix `wc` kommando: [https://man7.org/linux/man-pages/man1/wc.1.html](https://man7.org/linux/man-pages/man1/wc.1.html)
