---
title:    "Swift: Generering av slumpmässiga nummer"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en viktig del av många program och spel, det kan ge variation och utmaning till användare. Med hjälp av Swift kan du enkelt skapa och hantera slumpmässiga nummer för att ge ditt program en extra dimension av spänning och dynamik.

## Så här gör du

För att generera slumpmässiga nummer i Swift använder vi oss av random() funktionen. Vi kan ange ett intervall för vårt slumpmässiga nummer genom att använda closed range operator (t.ex. 1...10 för ett nummer mellan 1 och 10). Här är ett exempel på hur vi kan använda funktionen för att generera tre slumpmässiga nummer och skriva ut dem i konsolen:

```Swift
let randomNum1 = Int.random(in: 1...10)
let randomNum2 = Int.random(in: 1...10)
let randomNum3 = Int.random(in: 1...10)

print("Slumpmässiga nummer:", randomNum1, randomNum2, randomNum3)
```

Output:
```
Slumpmässiga nummer: 4 8 2
```

Om vi istället vill generera en decimaltal mellan 0 och 1, kan vi använda random() funktionen utan något intervall:

```Swift
let randomDouble = Double.random(in: 0...1)

print("Slumpmässigt decimaltal:", randomDouble)
```

Output:
```
Slumpmässigt decimaltal: 0.593455
```

Vi kan också använda random() funktionen för att generera slumpmässiga strängar från en lista av alternativ:

```Swift
let names = ["Anna", "Erik", "Maria", "Anders"]
let randomName = names.randomElement()

print("Slumpmässigt namn:", randomName)
```

Output:
```
Slumpmässigt namn: Anna
```

## Djupdykning

Random() funktionen i Swift använder sig av Mersenne Twister algoritmen för att generera slumpmässiga nummer. Detta är en av de mest populära algoritmerna inom datorvetenskap och ger hög kvalitet på de slumpmässiga nummer som genereras.

För att förhindra att samma sekvens av slumpmässiga nummer genereras varje gång programmet körs, används en seed (frö) som startvärde för algoritmen. I Swift genereras detta seed automatiskt, men vi kan också ange en egen seed för att få en specifik sekvens av slumpmässiga nummer.

Det finns också andra funktioner inom Swift som kan användas för att generera slumpmässiga nummer, som till exempel arc4random_uniform() och drand48().

## Se även

- [Apple Dokumentation för Randomization](https://developer.apple.com/documentation/swift/Randomization)
- [Swift Randomizer paket](https://github.com/SwifterSwift/SwifterSwift/blob/master/Sources/Foundation/Randomizer.swift)
- [Slumpmässiga nummer i Swift Playgrounds](https://www.raywenderlich.com/158106/random-numbers-in-swift-3-tutorial-for-ios)