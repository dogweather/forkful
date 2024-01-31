---
title:                "Søking og erstatting av tekst"
date:                  2024-01-20T17:58:50.903872-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Å søke og erstatte tekst betyr å finne spesifikke ord eller strenger og bytte dem ut med noe annet. Programmerere bruker dette for å oppdatere kode, behandle data, eller masseendre tekstfiler.

## How to:
Lua gjør søk og erstatt enkelt med `string.gsub`. Her er hvordan du gjør det:

```Lua
local originalText = "Hei verden. Verden er stor."
local searchText = "verden"
local replaceWith = "norge"
local resultText = string.gsub(originalText, searchText, replaceWith)
print(resultText) -- Output: Hei norge. norge er stor.
```

`gsub` returnerer to verdier: den endrede teksten og antall erstatninger:

```Lua
local replacements
resultText, replacements = string.gsub(originalText, searchText, replaceWith)
print("Antall erstatninger:", replacements) -- Output: Antall erstatninger: 2
```

Bruk mønstermatching for kompleksitet:

```Lua
local complexText = "Lua er gøy. Lua er kraftig."
local pattern = "Lua (.-) g"
resultText = string.gsub(complexText, pattern, "Lua er $1stilig og g")
print(resultText) -- Output: Lua er stilig ogøy. Lua er kraftig.
```

## Deep Dive
Søk og erstatte i Lua, som med mange script-språk, har røtter i tekstbehandlingsverktøy som `sed` i Unix. Lua's `string.gsub` er effektiv, men enkel sammenlignet med `sed`. Det tilbyr grunnleggende mønstermatching som ligner regulære uttrykk, men Lua's mønstre er ikke like kraftige eller komplekse som fullverdige regulære uttrykk.

Du kan også gjøre søk og erstatt ved hjelp av eksterne biblioteker som `lrexlib`, som tilbyr mer komplekse regulære uttrykk. Imidlertid, `string.gsub` dekker de fleste behov og er innebygd, så det er ingen ekstra avhengigheter.

For å implementere erstatning, går Lua gjennom teksten og ser etter mønstre. Ved hvert funn gjøres erstatningen, og søket fortsetter til det ikke finnes flere treff.

For ytelse, bør du bruke kompilerte mønstre hvis du gjør mange søk i store tekstmengder.

## See Also
- Lua's online documentation for string patterns: [String Patterns](https://www.lua.org/manual/5.4/manual.html#6.4.1)
