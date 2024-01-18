---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Lua: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden betyr rett og slett å programmere et script som kan regne ut en ny dato basert på en gitt dato og antall dager som skal legges til eller trekkes fra. Dette er nyttig for programmerere fordi det gjør det enklere å håndtere datoer i programvare og automatisere tidsrelaterte oppgaver.

## Hvordan:
Du kan beregne en dato i Lua ved å bruke funksjonen ```os.date``` og spesifisere en formatstreng og antall dager som skal legges til eller trekkes fra. Se eksempelet under for å se hvordan dette gjøres:
```
local dato = os.date("%m/%d/%Y", os.time() + 7*24*60*60) -- Beregner datoen 7 dager frem i tiden
print(dato) -- Printer ut datoen på ønsket format, i dette tilfellet "mm/dd/yyyy"
```
Eksempel output: ```05/16/2021```

## Dykk Deeper:
Historisk sett har beregning av datoer vært en utfordrende oppgave for programmerere, spesielt når det kommer til å håndtere forskjellige kalendersystemer og skuddår. Alternativt til å beregne datoer kan man også bruke en tidsmodul som ```os.time``` for å håndtere tidsstempel i sekunder siden 1. januar 1970. Implementeringen av beregning av datoer kan også variere mellom programmeringsspråk, så det kan være nyttig å være klar over forskjellene når man jobber med forskjellige språk.

## Se også:
- [Lua dokumentasjon om dato og tid](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Stack Overflow innlegg om beregning av dato i Lua](https://stackoverflow.com/questions/32553572/how-do-i-add-x-days-to-a-date-in-lua)
- [WikiHow-guide om å beregne datoer i Lua](https://www.wikihow.com/Add-Days-to-a-Date-in-Lua-Programming)