---
title:                "Go: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering eftersom det hjälper oss att hitta och fixa buggar tidigt i utvecklingsprocessen. Det bidrar också till en ökad kvalitet i vår kod och skapar ett robust system som är lättare att underhålla.

## Hur man gör det

För att skriva tester i Go använder vi paketet "testing". Vi startar med att importera paketet och sedan skapar vi en ny fil som heter "example_test.go". Inuti denna fil kan vi använda funktionen "func TestXXX(t *testing.T)" för att skriva våra tester. Här är ett exempel på en enkel testfunktion:

```Go
func TestAddition(t *testing.T) {
    result := addition(5, 3)
    if result != 8 {
        t.Errorf("Expected 8, got %v", result)
    }
}
```

Förutom att använda "t.Errorf()" kan vi också använda "t.Fatalf()" för att avbryta testningen om ett fel uppstår. Det finns också andra hjälpfunktioner som kan användas för att göra tester mer läsbara och intuitiva.

## Djupdykning

När vi skriver tester är det viktigt att tänka på olika scenarier och gränsvärden för våra funktioner. Genom att använda olika testfall kan vi säkerställa att vår kod fungerar korrekt i olika situationer. Det är också viktigt att kontrollera alla returvärden och eventuella felmeddelanden för att säkerställa att vår kod beter sig som förväntat.

Vi kan också använda "benchmarking" för att mäta prestanda på vår kod och se om den behöver optimeras. Detta kan vara särskilt viktigt för större projekt där prestanda är avgörande.

## Se även

- [Officiell dokumentation för "testing" paketet på Go's hemsida](https://golang.org/pkg/testing/)
- [En grundläggande guide om att skriva tester i Go](https://medium.com/@meeusdylan/unit-testing-in-go-a9361604b3ce)
- [En fördjupningsartikel om testning och benchmarking i Go](https://medium.com/@ananddanaraddi/testing-vs-benchmarking-in-go-ac31e84de7a4)