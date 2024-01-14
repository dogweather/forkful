---
title:    "Swift: Generering av slumpmässiga nummer"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är ett viktigt verktyg inom programmering. Det gör det möjligt att skapa spel, simuleringar och andra program där slumpmässighet är önskvärd.

## Hur man gör det

Att generera slumpmässiga nummer i Swift är enkelt. Först måste du importera Foundation-frameworket. Sedan kan du använda funktionen `arc4random_uniform()` för att generera ett slumpmässigt heltal inom ett givet intervall. Här är ett exempel på kod:

```Swift
import Foundation
// Genererar ett slumpmässigt heltal mellan 1 och 10
let randomInt = arc4random_uniform(10) + 1
print(randomInt) // Skriver ut det slumpmässiga talet
```

Output: 8

Du kan också använda funktionen `random()` för att generera ett slumpmässigt flyttal mellan 0 och 1. Här är ett exempel:

```Swift
let randomFloat = Float.random(in: 0..<1)
print(randomFloat)
```

Output: 0.76901865

Det finns också andra sätt att generera slumpmässiga nummer, till exempel genom att använda UUID() för att generera unika ID-nummer.

## Djupdykning

I Swift används en algoritm som kallas Mersenne Twister för att generera slumpmässiga nummer. Detta är en avancerad algoritm som är väldigt effektiv och ger en hög grad av slumpmässighet. Det finns också olika sätt att generera mer specialiserade slumpmässiga värden, till exempel slumpmässiga bokstäver eller bitar.

Slumpmässighet är också en viktig del av kryptografi och måste därför implementeras på ett säkert sätt för att undvika möjliga säkerhetshot.

## Se även

- [Foundation framework](https://developer.apple.com/documentation/foundation)
- [UUID()](https://developer.apple.com/documentation/swift/uuid) - för generering av unika ID-nummer
- [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister) - för djupare förståelse av den algoritm som används i Swift.