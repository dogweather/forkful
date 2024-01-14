---
title:    "Go: Skrive tester"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en erfaren Go-programmerer, har du sannsynligvis hørt om testing. Men hvorfor er det så viktig? Testing er en viktig del av utviklingsprosessen for å sikre at koden vår fungerer som den skal. Ved å skrive tester kan vi finne og fikse feil tidlig i utviklingsfasen, noe som sparer oss for mye tid og hodepine senere.

## Slik gjør du det

For å skrive tester i Go, må vi først importere pakken "testing". Deretter kan vi bruke funksjonene "t.Logf" og "t.Errorf" for å logge utdata og feilhenvisninger. La oss se på et eksempel:

```Go
package main

import (
    "testing"
    "math"
)

func TestSquareRoot(t *testing.T) {
    result := math.Sqrt(9)
    expected := 3.0

    if result != expected {
        t.Errorf("Expected %v, got %v", expected, result)
    } else {
        t.Logf("Success! The square root of 9 is %v", result)
    }
}
```

output:
```
--- FAIL: TestSquareRoot (0.00s)
    main_test.go:12: Expected 3.0, got 2.0
FAIL
exit status 1
FAIL    _/home/test    0.007s
```

I dette eksempelet tester vi om resultatet av kvadratroten skal være lik det forventede svaret 3.0. Hvis svarene ikke matcher, bruker vi "t.Errorf" for å vise en feilmelding. Hvis de matcher, bruker vi "t.Logf" for å vise en suksessmelding. Dette er en enkel test, men du kan implementere flere tester for å sikre at koden din fungerer i ulike scenarier.

## Fordypning

Å skrive effektive tester handler ikke bare om å teste logikk og funksjonalitet. Det er også viktig å teste grensetilfeller og feilhåndtering. Når du skriver tester, bør du prøve å dekke alle mulige scenarier for å sikre at din kode er robust og pålitelig.

En annen viktig del av testing er å mocke objekter og funksjoner. Ved å gjøre dette kan du simulere ulike situasjoner og kontrollere hva slags data og utdata er involvert i testene dine. Dette kan hjelpe deg med å finne og fikse feil som du kanskje ikke ville ha oppdaget ellers.

## Se også

- [Offisiell dokumentasjon for pakken "testing" i Go](https://golang.org/pkg/testing/)
- [Go Testing Cheat Sheet for quick reference](https://devhints.io/go-testing)
- [Eksempler på testing med Go](https://github.com/techtalk/go-testing-examples)