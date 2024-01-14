---
title:                "Go: Skapa slumpmässiga nummer"
simple_title:         "Skapa slumpmässiga nummer"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Generering av slumpmässiga tal är en vanlig uppgift inom programmering. Det kan användas för att skapa unika lösenord, testa algoritmer, eller simulera slumpmässiga händelser i ett program.

## Hur man gör det

För att generera slumpmässiga tal i Go, kan man använda funktionen `math/rand.Intn(n)`, där `n` är det största talet som kan genereras. Till exempel, för att generera ett slumpmässigt heltal mellan 1 och 10, kan man skriva:

```Go
slumpmässigtTal := rand.Intn(10) + 1 
fmt.Println(slumpmässigtTal)
```

Detta kommer att skriva ut ett slumpmässigt tal mellan 1 och 10 varje gång koden körs.

Man kan också använda `rand.Float64()` för att generera ett slumpmässigt flyttal mellan 0.0 och 1.0. För att få ett slumpmässigt tal mellan två specifika tal, till exempel 3.5 och 8.9, kan man lägga till en formel:

```Go
slumpmässigtTal := rand.Float64() * (8.9 - 3.5) + 3.5
fmt.Println(slumpmässigtTal)
```

Detta kommer att generera ett flyttal mellan 3.5 och 8.9 varje gång koden körs.

## Djupdykning

Vid generering av slumpmässiga tal, är det viktigt att ta hänsyn till "seed"-värdet. Detta är ett startvärde som används för att skapa serien av slumpmässiga tal. Om man inte anger ett seed-värde, kommer samma sekvens av slumpmässiga tal att genereras varje gång koden körs.

Genom att använda `rand.Seed(n)` kan man dock ändra på seed-värdet och få en annan sekvens av slumpmässiga tal. Detta är användbart om man till exempel vill testa en algoritm med olika slumpmässiga värden.

## Se även

* [Go's math/rand-paket](https://golang.org/pkg/math/rand/)
* [Go's fmt-paket](https://golang.org/pkg/fmt/)
* [Exempelkod på generering av slumpmässiga tal i Go](https://play.golang.org/p/-yR36TjXOgZ)