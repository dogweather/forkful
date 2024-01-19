---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att starta ett nytt projekt innebär att inleda skapandet av en ny programvara för att lösa specifika problem eller erbjuda en viss tjänst. Programmerare gör detta för att utveckla unik mjukvara som uppfyller specifika behov, vilket kan inkludera allt från att automatisera uppgifter till att tillhandahålla mer komplexa applikationer.

## Hur man gör:

För att starta ett nytt Haskell-projekt, behöver du ett grundläggande projekt skelett. Här har vi 'Hello, World!' exempel:

```Haskell
main :: IO ()
main = putStrLn "Hello, World!"
```
Kör din kod och du kommer att se följande utdata:
```Haskell
Hello, World!
```

För att skapa en mer komplex applikation, kan du använda 'Stack' som är ett verktyg som hjälper dig att skapa, testa och publicera ditt Haskell-program.

## Djupdykning: 

Haskell skapades 1990 och var en banbrytande programmeringsspråk med stark statisk typning, lat evaluering och ren funktionsprogrammering. 

Alternativ till Haskell inkluderar funktionella programmeringsspråk som Erlang, Elixir och Scala. Dessa tillhandahåller liknande funktionsprogrammeringskoncept, men har olika syntax och miljöegenskaper. 

För ytterligare implementation, överväg att använda byggverktyg och ramverk, såsom Stack och Cabal, för systematisk kodhantering, test och distribution.

## Se också:

1. Learn You A Haskell: (http://learnyouahaskell.com/)
2. Real World Haskell: (http://book.realworldhaskell.org/)
3. Haskell Wiki: (https://wiki.haskell.org/)
4. Stack: (https://docs.haskellstack.org/en/stable/README/) 

Lycka till med ditt nästa Haskell-projekt!