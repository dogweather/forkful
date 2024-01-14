---
title:                "Haskell: Att påbörja ett nytt projekt"
simple_title:         "Att påbörja ett nytt projekt"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Att starta ett nytt projekt kan vara en spännande och givande upplevelse för många utvecklare. Det ger en möjlighet att utforska nya idéer, lära sig nya tekniker och utvecklas som programmerare. Dessutom kan det vara ett sätt att lösa ett specifikt problem eller brist på resurser inom ett visst område.

## Så här gör du

För att komma igång med Haskell-programmering behöver du först installera ett Haskell-språkpaket och en Haskell-kompilator. Ett populärt sätt att göra detta på är att använda Haskell Platform, som innehåller allt du behöver för att köra Haskell-program.

När du väl har installerat Haskell-platformen kan du börja skriva ditt första program. Här är ett exempel på ett enkelt program som beräknar kvadratroten av ett tal:

```Haskell
main = do
  putStrLn "Skriv ett tal att ta kvadratroten av:"
  input <- getLine
  let number = read input :: Float
  let sqrtNumber = sqrt number
  putStrLn $ "Kvadratroten av " ++ show number ++ " är " ++ show sqrtNumber
```

Koden börjar med att skriva ut en instruktion och sedan tar emot input från användaren. Efter att input har tolkats och omvandlats till en flyttal, beräknas kvadratroten och slutligen skrivs resultatet ut.

Kör programmet genom att skriva`runhaskell program.hs` i din terminal. Om allt har gått rätt till kommer det att skrivas ut en instruktion och sedan kunna ta emot ditt tal och presentera kvadratroten.

## Djupdykning

Innan du börjar skriva ditt eget projekt är det viktigt att bekanta dig med Haskell-syntaxen och de grundläggande koncepten i språket. En bra resurs för detta är The Haskell Book, som är en omfattande guide till Haskell-programmering.

När du känner dig bekväm med grundläggande Haskell-programmering kan du börja utforska olika ramverk och bibliotek för att utveckla mer avancerade applikationer. Det finns många resurser online som kan hjälpa dig hitta rätt verktyg för ditt projekt och lära dig hur man använder dem.

## Se också

- [Haskell Platform](https://www.haskell.org/platform/)
- [The Haskell Book](http://haskellbook.com/)
- [Awesome Haskell](https://github.com/krispo/awesome-haskell) (en samling av användbara Haskell-resurser)