---
title:                "Sammanslagning av strängar"
html_title:           "Elm: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sammanfogning av strängar i programmering är processen att lägga ihop flera strängar för att skapa en längre sträng. Detta är användbart för att skapa dynamiskt innehåll på en webbplats eller för att skapa enkel textbas information till användare.

## Hur?
```Elm 
import Html

strang1 = "Hej "
strang2 = "Världen!"
kombinerad = strang1 ++ strang2

Html.text kombinerad
```

Output:
Hej Världen!

## Djupdykning:
Att sammanfoga strängar är en grundläggande funktion inom programmering och är vanligt förekommande i många olika programmeringsspråk. Det finns dock alternativa sätt att sammanfoga strängar, som att använda en "join" -funktion eller en "concat" -funktion. I Elm kan man också använda funktionen "append" för att sammanfoga strängar. 

När man sammanfogar strängar i Elm använder man operatorn "++" istället för "+" som man kanske är van vid från andra språk. Detta är för att i Elm är "+" reserverat för aritmetiska operationer. 

## Se även:
Officiell dokumentation för sträng-sammanfogning i Elm: https://guide.elm-lang.org/strings.html#concatenation