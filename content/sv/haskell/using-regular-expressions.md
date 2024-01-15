---
title:                "Användning av reguljära uttryck"
html_title:           "Haskell: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Reguljära uttryck är ett kraftfullt verktyg för strängmanipulering, vilket kan vara användbart i många olika situationer. Det kan användas för att söka efter mönster i en text, ersätta texter eller validera användarinput.

## Så här gör du
För att använda reguljära uttryck i Haskell måste du importera "Text.Regex.Posix" biblioteket i din kod. Sedan kan du använda funktionerna "makeRegex" och "match" för att skapa och tillämpa ditt reguljära uttryck. Här är ett exempel som söker efter alla delsträngar som innehåller siffror i en given sträng:

```Haskell
import Text.Regex.Posix (makeRegex, match)

main = do
 let str = "Det finns 25 elever i klassen."
 let regex = makeRegex "[0-9]+" :: Regex
 print $ match regex str
```

Detta kodexempel kommer att skriva ut en lista med träffar, vilket i detta fall kommer att vara ["25"]. Du kan också använda reguljära uttryck med funktioner som "subRegex" för att ersätta delsträngar eller "matchTest" för att validera en sträng mot det reguljära uttrycket.

## Djupdykning
En reguljär uttryck är en sträng av tecken som definierar ett mönster av tecken som kan matchas mot en annan sträng. Det finns många olika specialtecken som kan användas i reguljära uttryck för att specificera mönster. Till exempel representerar "." vilket som helst tecken och "[0-9]" representerar alla siffror.

För att lära dig mer om hur du kan använda reguljära uttryck i Haskell, kan du kolla in dokumentationen för "Text.Regex.Posix" biblioteket. Där hittar du en fullständig lista över funktioner och exempel på hur de kan användas.

## Se även
- [Haskell dokumentation för "Text.Regex.Posix"](https://hackage.haskell.org/package/regex-posix)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html)