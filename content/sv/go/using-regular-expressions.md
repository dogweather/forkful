---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönstermatchningstekniker för text. Programmerare använder det för att söka, validera, och manipulera data snabbt och effektivt.

## Hur gör man:
```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    re := regexp.MustCompile(`\w+@\w+\.\w+`)
    email := "exempel@doman.se"
    fmt.Println(re.MatchString(email)) // Ska skriva ut true om det är en matchning

    // Hitta alla matchningar
    text := "kontakt@exempel.se info@doman.se"
    emails := re.FindAllString(text, -1)
    fmt.Println(emails) // ["kontakt@exempel.se", "info@doman.se"]
}
```
Sample Output:
```
true
[kontakt@exempel.se info@doman.se]
```

## Fördjupning
Reguljära uttryck har funnits sedan 1950-talet, skapade av matematikern Stephen Cole Kleene. Alternativ till regex i Go kan vara att använda inbyggda strängfunktioner som `strings.Contains` eller att skriva egna parsers. Go använder RE2-lmotor, som är snabb och undviker vissa komplexa regexproblem som kan leda till prestandaproblem.

## Se även
- Go's regexp paket: https://golang.org/pkg/regexp/
- RE2-syntax: https://github.com/google/re2/wiki/Syntax
- Go by Example – Regular Expressions: https://gobyexample.com/regular-expressions
