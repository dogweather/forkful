---
title:                "Go: Å finne lengden på en streng"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til at man kan ha behov for å finne lengden til en tekststreng i et Go-program. Kanskje du ønsker å validere brukerinput, analysere data eller manipulere tekst på en bestemt måte. Uansett hva din grunn måtte være, er det viktig å forstå hvordan man kan finne lengden til en streng i Go.

## Slik gjør du det

For å finne lengden til en streng i Go, kan du bruke den innebygde funksjonen `len()`. La oss se på et enkelt eksempel:

```Go
package main

import "fmt"

func main() {
    tekst := "Hei, verden!"
    lengde := len(tekst)
    fmt.Printf("Lengden til strengen er %d.", lengde)
}
```

I dette eksempelet, definerer vi først en variabel `tekst` som inneholder strengen "Hei, verden!". Deretter bruker vi `len()`-funksjonen til å finne lengden til strengen og lagrer resultatet i variabelen `lengde`. Til slutt skriver vi ut lengden ved hjelp av `fmt.Printf()`.

```shell
Lengden til strengen er 12.
```

Som du kan se, returnerer `len()`-funksjonen antall tegn i en streng, inkludert mellomrom og spesialtegn. Så hvis du ønsker å finne antall bokstaver i en streng, må du kanskje foreta en tilpasning i koden din.

## Hvorfor "lengde" kan være feil begrep

Selv om det er vanlig å bruke ordet "lengde" når vi snakker om størrelsen på en streng, kan det være en misvisende betegnelse. Dette skyldes at Go bruker Unicode-tegnsettet, hvor noen tegn kan ha en lengde på mer enn 1 byte. Dermed kan det hende at `len()`-funksjonen returnerer et resultat som ikke stemmer overens med hva vi vanligvis tenker på som "lengden" av en streng. Derfor kan det være mer nøyaktig å si at `len()`-funksjonen returnerer antall tegn i en streng.

## Se også

- [Go sin offisielle dokumentside om `len()`-funksjonen](https://golang.org/ref/spec#Length_and_capacity)
- [En tutorial om å finne lengden på en streng i Go](https://www.dotnetperls.com/length-go)
- [En artikkel om Unicode-støtte i Go](https://blog.golang.org/strings)