---
title:                "Elm: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Vad är anledningen till att man skulle vilja veta längden på en sträng? Det finns flera användbara anledningar, till exempel att kontrollera om en sträng är för lång för att spara i en databas, eller för att kunna formatera text på ett korrekt sätt.

## Så här gör du
För att beräkna längden på en sträng i Elm, kan du använda funktionen `String.length` som tar en sträng som argument och returnerar en `Int` som representerar längden på strängen. Om vi till exempel vill beräkna längden på strängen "Hej", skulle vi skriva:
```Elm
String.length "Hej" --> 3
```

Denna funktion fungerar också på utf-8 kodade strängar, vilket betyder att den kan hantera specialtecken och emojis korrekt.

## Djupdykning
Om du vill gräva djupare och förstå hur denna funktion fungerar under ytan, kan vi titta på implementationen av `String.length` i Elm:s standardbibliotek. Där kan vi se att den använder sig av en `Utf8.length` funktion för att hantera utf-8 kodning och räkna antalet bytes som krävs för en sträng. Detta är ett bra exempel på hur Elm:s modell för text hanterar olika språk och teckenuppsättningar på ett pålitligt sätt.

# Se även
- [Elm:s dokumentation för String-modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Utf8-modulen](https://package.elm-lang.org/packages/elm/bytes/latest/Utf8)
- [En guide för att hantera text i Elm](https://dev.to/robinhunn/text-handling-and-unicode-awareness-in-elm-2n6l)