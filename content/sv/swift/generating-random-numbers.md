---
title:                "Generering av slumpmässiga tal"
html_title:           "Swift: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumpmässiga nummer är en vanlig uppgift för programmerare. Genom att slumpmässigt välja nummer kan vi skapa variation och realism i våra applikationer, spel och simuleringar.

## Hur gör man?
För att generera slumpmässiga nummer i Swift använder vi antingen den inbyggda funktionen `arc4random()` eller `arc4random_uniform()`. Här är ett exempel på hur man använder dessa funktioner:

```Swift
let randomNum = arc4random()
print("Slumpmässigt nummer: \(randomNum)")
//Output: Slumpmässigt nummer: 2631852051
```
```Swift
let randomNum = arc4random_uniform(100)
print("Slumpmässigt nummer mellan 0 och 99: \(randomNum)")
//Output: Slumpmässigt nummer mellan 0 och 99: 56
```

## Djupdykning
Att generera slumpmässiga nummer är en vanlig utmaning inom datavetenskap och används inom många olika områden som spel, simuleringar och kryptering. I äldre versioner av Swift användes funktionen `rand()` för att generera slumpmässiga nummer, men den är numer ersatt av `arc4random()` för att undvika förutsägbarhet i de genererade numren.

Det finns även andra sätt att generera slumpmässiga nummer i Swift, såsom användning av tredjepartsbibliotek eller implementering av egna algoritmer. Det är viktigt att vara medveten om datatyper och begränsningar när man använder slumpmässiga nummer, såsom att `arc4random()` endast kan användas för positiva heltal.

## Se även
Vill du lära dig mer om att generera slumpmässiga nummer i Swift? Kolla in Swifts dokumentation för de inbyggda funktionerna `arc4random()` och `arc4random_uniform()`: https://developer.apple.com/documentation/swift/1540993-arc4random