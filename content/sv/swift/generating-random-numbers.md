---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:50:13.073327-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
I Swift behöver vi ibland slumptal. Det kan vara för spellogik, säkerhet eller testdata. Slumptal gör våra program oförutsägbara och dynamiska.

## How to:
Genom att använda Swifts standardbibliotek är det enkelt att generera slumptal. Här är några exempel på hur du gör:

```Swift
// Slumpmässigt heltal mellan 0 och 9
let randomInt = Int.random(in: 0..<10)
print(randomInt)

// Slumpmässigt flyttal mellan 0 och 1
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)

// Slumpmässig boolean
let randomBool = Bool.random()
print(randomBool)
```
Exempel på output:
```
6
0.8654016930996748
true
```

## Deep Dive
Generering av slumptal i programmering är inte riktigt ”slumpmässig”. Det vi kallar slumptal genereras av en algoritm och är egentligen förutsägbara (*pseudoslumptal*). Swift använder en algoritm som skapar en sekvens av slumptal som är svåra att förutsäga. Dock, för verklig kryptografisk säkerhet, använd `SecRandomCopyBytes` från Security frameworket för att få kryptografiskt säkra slumptal.

På tiden anders Bordsdatorer blev alltmer populära, användes fysiska metoder som ljud eller elektronisk "brus" för att skapa slumptal. Dessa metoder är naturligt oförutsägbara och kallas för "sanna slumptal". 

Innan Swift fanns många sätt att generera slumptal i olika programmeringsspråk. Objektivt-C använde `arc4random` och dess varianter, medan C och C++ ofta har litar på `rand()` funktionen från standardbiblioteket men ofta kritiserats för dess förutsägbarhet och brist på uniform distribution.

Swifts nuvarande systemför slumptalsgeneratorer är mer flexibelt och tillförlitligt. Uniform random distribution är standard, vilket innebär att alla tal har lika stor chans att genereras. Utvecklare kan även skapa egna slumptalsgeneratorer genom att conforma till `RandomNumberGenerator` protokollet om de behöver ett speciellt beteende.

## See Also
För mer information och djupare detaljer, ta en titt på dessa källor:

- Apple's Swift documentation on random numbers: [Numbers and Basic Values](https://developer.apple.com/documentation/swift/numbers_and_basic_values)
- For cryptographically secure random numbers, see Apple's Security framework: [SecRandomCopyBytes](https://developer.apple.com/documentation/security/1399291-secrandomcopybytes)
