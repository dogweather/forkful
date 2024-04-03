---
date: 2024-01-20 17:38:15.675697-07:00
description: "Att konvertera en str\xE4ng till gemener inneb\xE4r att \xE4ndra alla\
  \ bokst\xE4ver till sm\xE5 bokst\xE4ver. Programmerare g\xF6r det f\xF6r att f\xF6\
  renkla j\xE4mf\xF6relser och\u2026"
lastmod: '2024-03-13T22:44:38.322642-06:00'
model: gpt-4-1106-preview
summary: "Att konvertera en str\xE4ng till gemener inneb\xE4r att \xE4ndra alla bokst\xE4\
  ver till sm\xE5 bokst\xE4ver."
title: "Konvertera en str\xE4ng till gemener"
weight: 4
---

## How to:
För att konvertera en sträng till gemener i Fish Shell kan du använda `string lower`.

```Fish Shell
set my_string "HeJ på DiG!"
string lower $my_string
```

Ovanstående kommando ger ut:

```
hej på dig!
```

## Deep Dive
Fish, en modern kommandotolk, har inbyggda strängoperationer. Förr var man tvungen att använda externa verktyg som `tr` eller `awk` för sådana uppgifter. I Fish utförs strängmanipulation internt och effektivt med funktioner som `string lower`. Det övergripande målet är att ge en enhetlig och skriptvänlig upplevelse.

Alternativ historiskt har inkluderat piping av output till `tr '[:upper:]' '[:lower:]'` eller att använda en extern applikation som `sed`. Dessa metoder fungerar men är inte lika snygga eller enkla som Fishs direkt integration.

Implementationsdetaljer är att `string lower` fungerar genom att ta emot en eller flera strängar och behandlar varje sträng individuellt, konverterar den till gemener. Funktionen har även extra flaggor som `-l` för att specificera en locale vilket är användbart i hantering av speciella tecken i olika språk.

## See Also
* officiell dokumentation: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
* Stack Overflow diskussioner: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)
