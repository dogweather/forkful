---
title:                "Skrive tester"
aliases: - /no/go/writing-tests.md
date:                  2024-02-03T18:15:05.890740-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive tester i Go innebærer å lage små, håndterbare deler av kode som validerer funksjonaliteten og oppførselen til din applikasjon. Programmerere skriver tester for å sikre at koden deres fungerer som forventet under ulike forhold, for å legge til rette for refaktorering, og for å hjelpe med å forhindre regresjoner.

## Hvordan:

I Go skrives tester typisk i samme pakke som koden de tester. Filer som inneholder tester er navngitt med suffikset `_test.go`. Tester er funksjoner som tar en peker til testing.T-objektet (fra `testing`-pakken) som et argument, og de signaliserer feil ved å kalle metoder som `t.Fail()`, `t.Errorf()`, osv.

Eksempel på en enkel test for en funksjon `Add` definert i `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Testfil `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    forventet := 3
    if result != forventet {
        t.Errorf("Add(1, 2) = %d; vil ha %d", result, forventet)
    }
}
```

Kjør testene dine med kommandoen `go test` i samme katalog som testfilene dine. Et eksempel på utdata som indikerer en bestått test ville se slik ut:

```
PASS
ok      example.com/my/math 0.002s
```

For tabelldrevne tester, som lar deg effektivt teste ulike inngangs- og utgangskombinasjoner, definer en skive med strukturer som representerer testtilfeller:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        forventet int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testnavn := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testnavn, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.forventet {
                t.Errorf("fikk %d, vil ha %d", ans, tt.forventet)
            }
        })
    }
}
```

## Dypdykk

Go-testrammeverket, introdusert i Go 1 samtidig som språket selv, ble designet for å integreres sømløst med Go-verktøysettet, noe som reflekterer Go sin vektlegging av enkelhet og effektivitet i programvareutvikling. I motsetning til noen testrammeverk i andre språk som er avhengige av eksterne biblioteker eller komplekse oppsett, gir Go sitt innebygde `testing`-pakke en enkel måte å skrive og kjøre tester på.

Et interessant aspekt ved Go sin tilnærming til testing er prinsippet om konvensjon over konfigurasjon det vedtar, som filnavnmønsteret (`_test.go`) og bruk av standardbibliotekfunksjoner over eksterne avhengigheter. Denne minimalistiske tilnærmingen oppmuntrer utviklere til å skrive tester, da barrieren for å starte er lav.

Selv om Go sine innebygde testfasiliteter dekker mye, er det scenarioer hvor tredjepartsverktøy eller rammer kan tilby mer funksjonalitet, som mock-generering, fuzz-testing, eller behavior-driven development (BDD)-stiltester. Populære biblioteker som Testify eller GoMock komplementerer Go's standard testingsevner, og tilbyr mer uttrykksfulle påstander eller mock-genereringskapabiliteter, noe som kan være spesielt nyttig i komplekse applikasjoner med mange avhengigheter.

På tross av eksistensen av disse alternativene, forblir den standard Go-testpakken hjørnesteinen for testing i Go på grunn av sin enkelhet, ytelse, og tett integrasjon med språket og verktøysettet. Enten utviklere velger å forsterke den med tredjepartsverktøy eller ikke, gir Go-testrammeverket et solid grunnlag for å sikre kodekvalitet og pålitelighet.
