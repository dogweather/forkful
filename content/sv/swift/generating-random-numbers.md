---
title:                "Generering av slumpmässiga nummer"
html_title:           "Swift: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

För många programmerare är det viktigt att ha möjligheten att generera slumpmässiga nummer i sina program. Det kan användas för spel, simuleringar, kryptering och mycket mer.

## Hur man gör det

Det finns flera olika sätt att generera slumpmässiga nummer i Swift. Det enklaste sättet är att använda funktionen `random(in:)`, som tar en intervall av värden som argument och returnerar ett slumpmässigt värde inom det intervallet. Här är ett exempel:

```Swift
let randomNumber = random(in: 1...6)
print(randomNumber) // Output: ett slumpmässigt heltal mellan 1 och 6
```

Vi kan också generera ett slumpmässigt flyttal med hjälp av `random(in:using:)` funktionen. Den tar inte bara ett intervall, utan också en instans av en generator som bestämmer hur slumpmässiga nummer ska genereras. Här är ett exempel:

```Swift
var generator = SystemRandomNumberGenerator()
let randomFloat = Double.random(in: 0...1, using: &generator)
print(randomFloat) // Output: ett slumpmässigt flyttal mellan 0 och 1
```

Slutligen kan vi använda `arc4random_uniform(_:)` funktionen som genererar ett heltal inom ett angivet intervall. Det är ett lättare alternativ till `random(in:)` funktionen som inte kräver speciella generatorinstanser men kan bara användas med heltal. Här är ett exempel:

```Swift
let randomNumber = arc4random_uniform(10)
print(randomNumber) // Output: ett slumpmässigt heltal mellan 0 och 9
```

## Djupdykning

Vad är då skillnaden mellan dessa tre sätt att generera slumpmässiga nummer i Swift? `random(in:)` och `random(in:using:)` utnyttjar båda samma generator som heter `XoroshiroRandomNumberGenerator`. Den är en kombination av två klassiska pseudoslumpmässiga generatorer och anses vara en av de bästa slumpmässiga generatorerna i Swift. Å andra sidan använder `arc4random_uniform(_:)` den äldre funktionen `arc4random()` från C och är inte lika pålitlig som de andra alternativen.

## Se även

- [Dokumentation för Swifts Random API](https://developer.apple.com/documentation/swift/random)
- [En djupgående studie om slumpmässiga generatorer i Swift](https://medium.com/@dgkostrub/swifts-randomness-api-arent-random-504a9fe0ff4c)
- [En guide för hur man använder Swifts Random API](https://www.hackingwithswift.com/articles/194/how-to-use-springs-random-number-generators-in-swift)