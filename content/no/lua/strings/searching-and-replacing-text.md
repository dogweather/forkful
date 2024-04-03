---
date: 2024-01-20 17:58:50.903872-07:00
description: "\xC5 s\xF8ke og erstatte tekst betyr \xE5 finne spesifikke ord eller\
  \ strenger og bytte dem ut med noe annet. Programmerere bruker dette for \xE5 oppdatere\
  \ kode,\u2026"
lastmod: '2024-03-13T22:44:40.914655-06:00'
model: gpt-4-1106-preview
summary: "\xC5 s\xF8ke og erstatte tekst betyr \xE5 finne spesifikke ord eller strenger\
  \ og bytte dem ut med noe annet."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

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
