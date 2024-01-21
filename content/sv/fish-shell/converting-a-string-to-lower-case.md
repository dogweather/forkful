---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:38:15.675697-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla bokstäver till små bokstäver. Programmerare gör det för att förenkla jämförelser och sökningar, då det tar bort skillnaden mellan stora och små bokstäver.

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