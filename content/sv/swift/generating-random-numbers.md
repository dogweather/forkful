---
title:                "Swift: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga siffror är ett vanligt behov inom programmering, särskilt inom spelutveckling och simuleringar. Genom att använda sig av slumpmässighet i programmet kan man skapa en mer varierande och spännande upplevelse för användaren.

## Hur man gör det
För att generera slumpmässiga siffror i Swift används funktionen `arc4random_uniform()` tillsammans med `Int()` för att få ett heltal. Till exempel kan man skriva följande kod för att generera ett slumpmässigt tal mellan 1 och 100:

```Swift
let randomNumber = Int(arc4random_uniform(100)) + 1
print(randomNumber) // utskrift: ett tal mellan 1 och 100
```

Man kan även använda sig av `arc4random()` för att få ett slumpmässigt tal inom ett visst intervall, till exempel mellan 5 och 10:

```Swift
let randomNumber2 = Int(arc4random()) % 6 + 5
print(randomNumber2) // utskrift: ett tal mellan 5 och 10
```

## Djupdykning
`arc4random_uniform()` och `arc4random()` fungerar genom att använda sig av en algoritm för att generera ett pseudoslumpmässigt tal baserat på en startpunkt och en multiplikator. Detta innebär att resultatet blir förutsägbart om man känner till startpunkten och multiplikatorn. Därför är det viktigt att inte använda sig av samma startpunkt varje gång man vill generera slumpmässiga tal.

För att undvika detta kan man använda t.ex. aktuell tid som startpunkt, eller en kombination av olika variabler.

## Se även
- [Apples dokumentation om `arc4random_uniform()`](https://developer.apple.com/documentation/swift/uint32/2995649-arc4random_uniform)
- [Mer information om pseudosumpmässighet och algoritmer](https://www.computerhope.com/issues/ch001461.htm)
- [Exempel på hur man kan använda slumpmässighet i spelutveckling med Swift](https://www.raywenderlich.com/3137782-game-logic-using-randomization-within-your-apple-game-frameworks-game-project)