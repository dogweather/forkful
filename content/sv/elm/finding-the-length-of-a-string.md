---
title:                "Hitta längden på en sträng"
html_title:           "Elm: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hitta längden på en sträng är en mycket användbar kunskap inom programmering. Det kan hjälpa dig att manipulera och hantera data på ett effektivt sätt och göra din kod mer läsbar och överskådlig.

## Hur man gör det
För att hitta längden på en sträng i Elm kan du använda funktionen `String.length`. Här är ett enkelt exempel på hur det skulle se ut i praktiken:

```Elm
import String exposing (length)

myString = "Hej, världen!"
length myString
-- Output: 13
```

I det här exemplet importerar vi funktionen `length` från Elm's `String` modul och använder den sedan för att hitta längden på vår sträng `myString`. Det returnerade värdet, 13, är antalet tecken i strängen.

## Djupdykning
När du använder `String.length` i Elm måste du vara medveten om att den returnerar antalet tecken i en sträng, inte antalet ord. Detta kan vara viktigt att komma ihåg när du bearbetar data.

En annan viktig punkt att notera är att `String.length` endast fungerar på strängar, inte på andra typer av data. Om du vill hitta längden på en lista eller ett annat datatype måste du använda en annan funktion.

## Se även
- Elm's String modul: https://package.elm-lang.org/packages/elm-lang/core/latest/String
- Elm's documentation om String: https://package.elm-lang.org/packages/elm-lang/core/latest/String