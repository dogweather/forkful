---
title:    "Haskell: Läsning av kommandoradsargument"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig del av att skriva effektiva Haskell-program. Genom att förstå hur man kan läsa dessa argument kan du göra dina program mer flexibla och anpassningsbara.

## Hur man gör det

Låt oss titta på ett enkelt exempel på hur man kan läsa kommandoradsargument i Haskell:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Det första argumentet är: " ++ head args
```

I detta exempel använder vi `System.Environment` modulen för att kunna använda funktionen `getArgs`, som hämtar en lista på alla kommandoradsargument som skickats till vårt program. Vi tilldelar denna lista till variabeln `args` och använder sedan `head` för att hämta det första argumentet. Därefter använder vi `putStrLn` för att skriva ut det på skärmen.

Om vi kör detta program med kommandoradsargumentet "Haskell" kommer följande att skrivas ut:

```
Det första argumentet är: Haskell
```

## Djupdykning

Det finns några saker att tänka på när man arbetar med kommandoradsargument i Haskell:

- `getArgs` returnerar alltid en lista, även om det inte finns några argument. För att undvika ett felmeddelande kan man använda `null` för att kontrollera om listan är tom.
- När man jobbar med flera argument kan man använda `tail` för att hämta en lista på alla argument utom det första.
- Det är också möjligt att använda `getProgName` för att hämta namnet på det exekverande programmet.

## Se även

- [Haskell Wikibook - Hantering av kommandoradsargument](https://en.wikibooks.org/wiki/Haskell/Handling_command-line_arguments)
- [Haskell-dokumentation - System.Environment modulen](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)