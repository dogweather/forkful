---
date: 2024-01-20 17:47:24.069875-07:00
description: "How to: Elm g\xF6r det enkelt med `String.length` funktionen. H\xE4\
  r \xE4r ett exempel."
lastmod: '2024-03-13T22:44:37.820364-06:00'
model: gpt-4-1106-preview
summary: "Elm g\xF6r det enkelt med `String.length` funktionen."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## How to:
Elm gör det enkelt med `String.length` funktionen. Här är ett exempel:

```Elm
import Html

main =
    Html.text (String.fromInt (String.length "Hej, Sverige!"))

-- Utskrift: "13"
```

Notera att `String.length` returnerar en `Int`.

## Deep Dive
Förr i tiden kunde teckenräkningsfunktioner ha prestandaproblem, eftersom de behövde iterera över varje tecken i strängen. Elm's `String.length` är optimerad och snabb, eftersom Elm använder utf-16 kodning där varje tecken i de flesta fall tar upp samma mängd minnesutrymme. Alternativ för att hitta en strängs längd innefattar att skriva en egen rekursiv funktion eller att använda en loop, men dessa metoder är onödiga och oftast mindre effektiva än den inbyggda `String.length`.

## See Also
För mer information, utforska Elm's officiella dokumentation för strängmodulen:
- [Elm String Module Documentation](https://package.elm-lang.org/packages/elm/core/latest/String#length)

Du kan också hitta mer fördjupande förklaringar och diskussioner kring strings och deras hantering i Elm på följande länkar:
- [Elm Discourse - String Manipulation](https://discourse.elm-lang.org/)
