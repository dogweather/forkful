---
title:                "Att Stor Bokstavera en Sträng"
html_title:           "Haskell: Att Stor Bokstavera en Sträng"
simple_title:         "Att Stor Bokstavera en Sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kapitalisera en sträng är en vanlig operation i programmering, speciellt när vi ska visa user input eller returnera data i större bokstäver. Genom att lära sig hur man kapitaliserar en sträng i Haskell, kan du öka din förståelse för språkets syntax och funktioner.

## Hur man gör
För att kapitalisera en sträng i Haskell, kan vi använda funktionen `toUpper` från modulen `Data.Char` tillsammans med en loop för att gå igenom varje tecken i strängen. Se nedan för ett exempel:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = [toUpper x | x <- str]

main = do 
    putStrLn "Skriv in en sträng: "
    str <- getLine
    putStrLn ("Kapitaliserad sträng: " ++ capitalize str)
```

**Input:** hej världen

**Output:** HEJ VÄRLDEN

I koden ovan definierar vi en funktion `capitalize` som tar emot en sträng och använder `toUpper` för att omvandla varje tecken i strängen till dess motsvarande stora bokstav. Sedan använder vi `getLine` för att ta emot en sträng från användaren och skriver ut den kapitaliserade versionen till konsolen. 

## Djupdykning
En intressant egenskap hos Haskell är möjligheten att definiera funktioner som tar emot andra funktioner som argument. Detta kallas för högre ordningens funktioner. Vi kan dra nytta av detta när vi vill kapitalisera en sträng genom att använda funktionen `map` istället för en loop. Se nedan för en alternativ implementation av `capitalize`:

```Haskell
capitalize :: String -> String
capitalize = map toUpper
```

Funktionen `map` tar emot en funktion och en lista som argument och applicerar sedan funktionen på varje element i listan för att returnera en ny lista med resultatet. I vårt fall tar `map` emot `toUpper` och applicerar den på varje tecken i strängen för att returnera en kapitaliserad version. 

## Se också
- [Haskell.org](https://www.haskell.org/): Officiell hemsida för Haskell med dokumentation och resurser för nybörjare.
- [Hoogle](https://haskell.org/hoogle/): En sökmotor för Haskell-funktioner som gör det enkelt att hitta rätt funktion för dina behov.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters): En online-bok som tar dig igenom Haskell-steg för steg.