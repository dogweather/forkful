---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sammanslagning av strängar är en operation där två eller fler strängar kopplas samman till en enda sträng. Programmerare gör detta för att snabbt bygga upp komplexa strängar från mindre delar.

## Hur man gör:
Här är exempel på kod och utdata i Gleam:

```Gleam
let fornamn = "Kalle"
let efternamn = "Anka"
let helsing = fornamn ++ " " ++ efternamn
```

Denna kod skapar en ny sträng `helsing` som kommer vara "Kalle Anka".

```Gleam
let text = "Kalle" ++ "Anka"
```

Denna kod skapar en ny sträng `text` som kommer vara "KalleAnka".

## Djupdykning
(1) Historiskt sett har sammanslagning av strängar varit en grundläggande operation i många programmeringsspråk, inklusive tidiga versioner av C och FORTRAN. (2) Alternativt kan du använda formaterade strängfunktioner som `sprintf` i C eller `intertext` i Gleam för mer komplex strängmanipulation. (3) Implementationen av strängsammanslagning kan variera stort beroende på programmeringsspråket. I Gleam är `++` en inbyggd funktion för att sammanfoga strängar.

## Se även
1. Officiell Gleam-dokumentation: https://gleam.run/
2. Gleams intext-funktion: [Gleam-intertext-dokumentation](https://hexdocs.pm/intertext/readme.html)
3. En djupdykning i strängsammanslagningar: [Strängsammanslagning på Wikipedia](https://en.wikipedia.org/wiki/Concatenation)