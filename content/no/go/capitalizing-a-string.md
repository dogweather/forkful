---
title:                "Store bokstaver i en streng"
html_title:           "Go: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å kunne kapitalisere en streng (string) er en viktig funksjon i ethvert programmeringsspråk. Det lar deg formatere tekst på en mer lesbar og strukturert måte.

## Hvordan kapitalisere en streng i Go?

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Opprett en streng (string) som skal kapitaliseres
    tekst := "dette er en tekst som skal kapitaliseres"

    // Bruk strings.Title() funksjonen for å kapitalisere strengen
    kapitalisertTekst := strings.Title(tekst)

    // Skriv ut resultatet
    fmt.Println(kapitalisertTekst)

    // Output: Dette Er En Tekst Som Skal Kapitaliseres
}
```

## Dypdykk i kapitalisering av strenger i Go

I tillegg til `strings.Title()` funksjonen, kan du også bruke `strings.ToUpper()` for å kapitalisere en streng. Forskjellen mellom disse to er at `strings.Title()` også endrer store bokstaver midt i en streng til små bokstaver.

Det finnes også andre måter å kapitalisere strenger på, som å bruke en `for`-løkke eller `map` funksjonen. Det er også viktig å være klar over at strings i Go er immutabel, så når du gjør en endring på en streng må du lagre resultatet i en ny variabel.

## Se også

- [Golang.org](https://golang.org/) - Offisiell hjemmeside for Go
- [Go by Example: Strings](https://gobyexample.com/) - Gode eksempler på hvordan håndtere strenger i Go