---
title:    "Go: Skriving av tester"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Hvorfor
Så hvorfor bry seg med å skrive tester? Vel, for det første bidrar det til å sikre at koden vår fungerer som den skal. Ved å skrive tester får vi også en innebygd dokumentasjon av koden vår, som kan hjelpe fremtidige utviklere å forstå hva koden gjør og hvordan den fungerer.

# Hvordan
For å skrive tester i Go-språket, bruker vi pakken "testing" som kommer med standardbiblioteket. La oss se på et enkelt eksempel:

```
package main

import "testing"

func add(x, y int) int {
    return x + y
}

func TestAdd(t *testing.T) {
    result := add(2, 3)
    if result != 5 {
        t.Errorf("Expected 5, got %d", result)
    }
}
```

Her har vi definert en enkel funksjon "add" som legger sammen to tall, og vi tester den ved å bruke Test-funksjonen fra "testing" pakken. Ved å bruke "if" uttrykk i Test-funksjonen, sammenligner vi resultatet av vår add funksjon med forventet verdi og gir en feilmelding hvis de ikke er like.

```
$ go test
ok      _/home/user/example     0.004s
```

Vi kan se at testen har blitt utført og resultatet var vellykket. Hvis vi endrer funksjonen vår slik at den returnerer et annet resultat, vil testen feile og gi oss en feilmelding.

# Dypdykk
Å skrive gode tester handler om å ha god dekning av koden vår og å teste alle mulige tilfeller. Man kan også bruke "benchmarks" for å måle ytelsen til koden vår. Go har også en funksjon som heter "coverage" som lar oss se hvor mye av koden vår som er dekket av testene våre.

Et annet nyttig verktøy er "mocking", som lar oss etterligne visse funksjoner eller objekter i testmiljøet vårt. Dette kan være spesielt nyttig når vi trenger å teste funksjoner som er avhengige av eksterne tjenester eller databasekall.

# Se også
- [Go Dokumentasjon: Testing](https://golang.org/pkg/testing/)
- [En kort introduksjon til testing i Go](https://blog.alexellis.io/golang-writing-unit-tests/)
- [Bli en bedre Go-utvikler gjennom testing](https://dev.to/elastic/become-a-better-go-developer-through-testing-1f65)