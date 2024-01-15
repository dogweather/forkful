---
title:                "Skriva tester"
html_title:           "Go: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av utvecklingsprocessen. Det hjälper till att säkerställa att koden fungerar som det är tänkt och minskar risken för buggar och fel i produktion. Det kan också öka kvaliteten på koden och underlätta för samarbete mellan utvecklare.

## Så här gör du

För att skriva tester i Go behöver du använda paketet "testing". Först måste du skapa en fil som slutar på "_test.go" för att testerna ska köras automatiskt av Go. Därefter kan du skapa en funktion för varje testfall som du vill köra. Här är ett exempel på en testfil som testar en funktion för att lägga till två tal:

```Go
package main_test

import "testing"

func TestAddition(t *testing.T) {
    sum := add(2, 3)
    expected := 5
    if sum != expected {
        t.Errorf("Summan av 2 och 3 är fel. Förväntat: %d, fick: %d", expected, sum)
    }
}

func add(a, b int) int {
    return a + b
}
```

När du kör testet kommer du att se följande utskrift:

```
ok      command-line-arguments  0.002s
```

Detta betyder att testet lyckades och alla dina funktioner fungerar som de ska. Om något test fallerar får du en detaljerad utskrift med information om felet, vilket hjälper dig att hitta och lösa problemet.

## Deep Dive

När du skriver tester är det viktigt att täcka alla möjliga scenarion av din kod. Detta inkluderar också felhantering och gränsvärden. Det kan också vara användbart att använda Go's inbyggda benchmarking-funktioner för att mäta prestanda av din kod.

Det är också möjligt att använda externa testramverk som ginkgo för att organisera och köra dina tester på ett mer strukturerat sätt.

## Se också

- https://golang.org/pkg/testing/
- https://onsi.github.io/ginkgo/