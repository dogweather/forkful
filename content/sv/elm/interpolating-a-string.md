---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Interpolering av strängar handlar om att infoga variabler eller uttryck inom en sträng. Programmerare gör detta för att sammansätta dynamisk text på ett mer läsbart och effektivt sätt.

## Hur man gör:

Elm erbjuder inte inbyggd stränginterpolering som många andra språk. Men du kan åstadkomma detta genom Elm's funktioner för strängkonkatenering (sammanslutning). Kolla på följande exempel:

```Elm
name = "Anna"
greeting = "Hej " ++ name ++ ", trevligt att se dig!"
```

Om du kör detta program kommer utskriften vara: `"Hej Anna, trevligt att se dig!"`.

## Fördjupning

Stränginterpolering har funnits i programmering i decennier och finns i många moderna språk som JavaScript och Python. Alternativt till		      konkatenering, kan man använda `String.concat` eller `String.join` för att sammansätta strängar.

Till exempel, här är hur du skulle göra det med `String.join`:

```Elm
name = "Anna"
parts = ["Hej ", name, ", trevligt att se dig!"]
greeting = String.join "" parts
```

Elm's filosofi prioriterar enkelhet och prediktabilitet, så det erbjuder inte direkt stränginterpolering - därmed behöver vi använda dessa metoder istället.

## Se Även

För mer information om strängmanipulation i Elm, se den officiella dokumenation av [`String` modulet](https://package.elm-lang.org/packages/elm/core/latest/String). För att förstå mer om varför Elm inte har inbyggd stränginterpolering, kan denna [tråd på Elm Discourse](https://discourse.elm-lang.org/t/string-interpolation/232) vara intressant.