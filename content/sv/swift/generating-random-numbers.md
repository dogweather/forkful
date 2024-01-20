---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Skapa slumpmässiga tal i Swift: En djupdykning 

## Vad & Varför?

Slumpmässig nummergenerering innebär att skapa tal som inte har något tydligt mönster eller sekvens. Programmerare använder detta för att införa oförutsägbarhet i deras applikationer, exempelvis i spel för att skapa en ovisshet.

## Hur man gör:

I Swift kan du generera ett slumpmässigt tal mellan två gränser genom att använda `Int.random(in:)`:

```Swift
let randomNum = Int.random(in: 1..<10)
print(randomNum)
```

När du kör detta kod, kommer du få ett tal mellan 1 och 9 (observera att övre gränsen inte ingår).

## Djupdykning 

Att generera slumpmässiga tal är ingenting nytt och har använts sedan de första dagarna av programmering. Swift förbättrade processen jämfört med tidigare språk genom att allmänt låta programmerare ange övre och nedre gränser för slumpmässiga tal, något många andra språk inte stöder i standardbiblioteket.

Ett alternativ till Swifts inbyggda metoder att skapa slumpmässiga tal är att använda en tredjepartsbibliotek som till exempel GameplayKit, som ger mer sofistikerade funktioner för att skapa slumpmässiga tal.

Det är viktigt att notera att Swift's slumpmässiga nummergenerator är tillräckligt bra för de flesta ändamål, men det kanske inte är tillräckligt robust för mycket känsliga tillämpningar, som kryptografi eller säkerhet. I sådana fall bör du överväga att använda mer avancerade tekniker, såsom kryptografiskt säkra pseudoslump Generatorer.

## Se också:

2. [Apple's GameplayKit](https://developer.apple.com/documentation/gameplaykit)
3. [Kryptografiskt säkra pseudoslump Generators](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator).