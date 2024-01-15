---
title:                "Generera slumpmässiga tal"
html_title:           "Go: Generera slumpmässiga tal"
simple_title:         "Generera slumpmässiga tal"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en nyckelkomponent inom programmering som tillåter utvecklare att inkludera element av slump och variation i sina program. Det är användbart i spel, simuleringar, kryptografi och många andra applikationer.

## Hur man gör
Generering av slumpmässiga nummer görs enkelt i Go genom användning av standardbiblioteket "math/rand". För att skapa ett slumpmässigt heltal mellan 0 och ett given tal, använd "rand.Intn(n)", där n är det övre gränsvärdet. För att generera ett flyttal mellan 0 och 1, använd "rand.Float64()". Nedan följer ett exempel på hur man kan använda detta i kod:

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    // Generera ett slumpmässigt heltal mellan 1 och 10
    randomInt := rand.Intn(10) + 1
    fmt.Println(randomInt) // Output: till exempel 7

    // Generera ett slumpmässigt flyttal mellan 0 och 1
    randomFloat := rand.Float64()
    fmt.Println(randomFloat) // Output: till exempel 0.734123455
}
```

Att sätta ett seed-värde (startvärde) är också viktigt för att få en varierad sekvens av slumpmässiga nummer. Detta kan göras genom att använda "rand.Seed()". Om seed-värdet inte är satt, kommer Go att använda samma seed-värde varje gång programmet körs, vilket leder till att samma nummer alltid genereras.

## Djupdykning
Math/rand-paketet i Go använder en algoritm som kallas "Linear Congruential Generator" för att generera slumpmässiga nummer. Detta är en enkel algoritm som använder en linjär ekvation för att skapa en lång sekvens av nummer baserat på ett initialt seed-värde. Det finns dock vissa begränsningar med detta, som att det är möjligt att förutsäga nästa nummer som kommer genereras om man känner seed-värdet och algoritmen. Det finns mer avancerade metoder för att generera slumpmässiga nummer baserat på kryptografiska algoritmer om större säkerhet behövs.

## Se även
- Officiell dokumentation för math/rand-paketet: https://golang.org/pkg/math/rand/
- Tutorial för att generera slumpmässiga nummer i Go: https://www.digitalocean.com/community/tutorials/how-to-use-the-math-rand-package-in-go
- Implementation av kryptografiskt säkra slumpmässiga nummer i Go: https://github.com/dgryski/trifles/tree/master/go/randutil