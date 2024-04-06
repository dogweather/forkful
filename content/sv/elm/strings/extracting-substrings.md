---
date: 2024-01-20 17:45:38.760040-07:00
description: "How to: (Hur g\xF6r man?) Elm h\xE5ller det enkelt. H\xE4r \xE4r ett\
  \ exempel p\xE5 hur du extraherar delstr\xE4ngar med funktionen `String.slice`."
lastmod: '2024-04-05T21:53:39.152450-06:00'
model: gpt-4-1106-preview
summary: "(Hur g\xF6r man?) Elm h\xE5ller det enkelt."
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## How to: (Hur gör man?)
Elm håller det enkelt. Här är ett exempel på hur du extraherar delsträngar med funktionen `String.slice`:

```Elm
import String exposing (slice)

-- Vi ska plocka ut "världen" från strängen "Hej världen!"
main =
  let
    fullText = "Hej världen!"
    partText = slice 4 11 fullText
  in
  -- partText blir "världen"
  text partText
```

Kör koden, och du får en enkel output: "världen".

## Deep Dive (Djupdykning)
I tidiga programmeringsspråk som C måste du jobba direkt med minnet för att hantera strängar, vilket var knepigare och mer felbenäget. Elm, som är modernare, döljer komplexiteten och låter oss göra sådant här lätt och säkert. 

Alternativen finns. Du kan använda `String.left` och `String.right` för att beskära strängar från start eller slut istället för `String.slice`. Dock är `slice` mer flexibel.

Angående implementation, Elm hanterar strängar internt som UTF-16, vilket är viktigt att ha i åtanke om du arbetar med tecken utanför ASCII. Det kan påverka indexeringen eftersom vissa tecken kan ta mer än ett "ord" i minnet.

## See Also (Se även)
- Elm's officiella dokumentation om strängar: https://package.elm-lang.org/packages/elm/core/latest/String
- En genomgång av strängmanipulation i Elm: https://elmprogramming.com/string.html
- UTF-16-teckenuppslag: https://en.wikipedia.org/wiki/UTF-16
