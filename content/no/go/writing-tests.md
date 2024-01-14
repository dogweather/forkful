---
title:                "Go: Skriving av tester"
simple_title:         "Skriving av tester"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor 

Å skrive tester er en viktig del av å lage gode Go-programmer. Tester hjelper deg med å oppdage og fikse feil i koden før den blir utgitt, og sikrer at koden din fungerer som den skal. Det er også en god praksis å følge i programmering, og viser at du bryr deg om kvaliteten på arbeidet ditt.

## Hvordan 

For å skrive tester i Go, kan du bruke standardbiblioteket "testing" som følger med språket. Her er et eksempel på hvordan en enkel test kan se ut:

```Go
func Sum(a, b int) int {
   return a + b
}

func TestSum(t *testing.T) {
   result := Sum(2, 3)
   if result != 5 {
      t.Errorf("Sum should be 5, got %d instead", result)
   }
}
```

Output fra denne testen vil være:

```Go
--- FAIL: TestSum (0.00s)
    main_test.go:11: Sum should be 5, got 6 instead
```

For å kjøre alle testene i pakken kan du bruke følgende kommando i terminalen:

```
go test
```

## Dypdykk 

Det finnes flere metoder for å skrive tester i Go, som for eksempel å bruke "subtests" for å dele opp en stor test i mindre deler. Det er også mulig å bruke tredjeparts biblioteker som "Testify", som gir deg flere verktøy for å skrive tester. Det er viktig å huske på at koden din bør være godt strukturert og testbar for å kunne skrive effektive tester.

## Se også 

- [testing package - Go standardbibliotek](https://golang.org/pkg/testing/)
- [Testify - tredjeparts testbibliotek for Go](https://github.com/stretchr/testify)