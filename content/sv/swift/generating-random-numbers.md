---
title:                "Swift: Generera slumpmässiga tal"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga tal är en användbar färdighet för programmerare eftersom det kan användas för att skapa spel, lotterispel eller slumpmässiga simuleringar. Det kan också vara användbart för att testa kod och lösa matematiska problem.

## Hur man gör

För att generera slumpmässiga tal i Swift, använd funktionen `arc4random()` som genererar ett slumpmässigt heltal. Om du vill generera ett slumpmässigt tal inom ett visst intervall kan du använda `arc4random_uniform()` som tar ett heltal som argument och genererar ett slumpmässigt tal mellan 0 och det angivna talet. Här är ett exempel på hur man ska använda dessa funktioner:

```Swift
// Generera slumpmässigt heltal mellan 1 och 10
let randomNum = arc4random_uniform(10) + 1
// Generera slumpmässigt heltal mellan 500 och 1000
let randomNum2 = arc4random_uniform(501) + 500

// Generera slumpmässigt nummer mellan 0 och 1
let randomDecimal = Double(arc4random()) / Double(UINT32_MAX)
```

I det här exemplet kommer `randomNum` att vara ett slumpmässigt tal mellan 1 och 10 och `randomNum2` kommer att vara ett slumpmässigt tal mellan 500 och 1000. Den sista raden i exemplet visar hur man kan generera ett slumpmässigt decimaltal genom att först generera ett heltal och sedan dela det med det högsta möjliga heltal för att få ett decimaltal mellan 0 och 1.

## Djupdykning

Om du vill kontrollera vilken typ av värde som `arc4random()` eller `arc4random_uniform()` genererar, kan du använda funktionen `type(of: )` för att se det faktiska värdet. Till exempel, om du vill göra ett slumpmässigt val mellan två strängar, kan du göra det med hjälp av `arc4random()` på följande sätt:

```Swift
let options = ["Välj mig!", "Inte jag!"]
let randomIndex = Int(arc4random_uniform(2))
print(type(of: options[randomIndex])) //String
print(options[randomIndex]) //Antingen "Välj mig!" eller "Inte jag!"
```

`options[randomIndex]` kommer att returnera antingen den första eller andra strängen, beroende på vilket slumpmässigt heltal som genererades. Användningen av `type(of: )` visar att det faktiska värdet som returneras är en sträng. Det här kan vara användbart när du arbetar med mer komplexa datastrukturer och vill försäkra dig om att rätt typ av värden hanteras.

## Se även

- [How to Use Random Numbers in Swift](https://www.hackingwithswift.com/example-code/system/how-to-use-random-numbers-in-swift)
- [Generating Random Numbers in Swift with GameplayKit](https://www.natashatherobot.com/generating-random-numbers-swift-gameplaykit/)