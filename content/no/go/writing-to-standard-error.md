---
title:                "Go: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive til standard error er en viktig del av Go-programmering, spesielt når du feilsøker og tester koden din. Det lar deg sende feilmeldinger og andre beskjeder til programmeringsmiljøet, noe som kan hjelpe deg med å finne og løse eventuelle problemer.

## Hvordan
For å skrive til standard error i Go, bruker du en innebygd funksjon kalt `fmt.Fprintf`. Her er et eksempel på hvordan du kan bruke dette i koden din:

```Go
package main

import "fmt"

func main() {
    fmt.Fprintf(os.Stderr, "Dette er en feilmelding: %s", "Ugyldig input")
}
```
Outputen av denne koden vil være:

```console
Dette er en feilmelding: Ugyldig input
```

Som du kan se, blir meldingen sendt til standard error i stedet for standard output. Dette kan være nyttig når du for eksempel kjører koden fra en terminal og ønsker å skille ut feilmeldinger fra ordinær output.

## Dypdykk
Når du skriver til standard error, er det viktig å huske på at meldingen blir sendt til det samme stedet som andre feilmeldinger i koden din. Dette betyr at du kan bruke funksjoner som `panic` eller `log.Fatal` for å håndtere disse meldingene på en effektiv måte. Det er også viktig å sørge for at meldingen er tydelig og beskrivende, slik at du og andre utviklere enkelt kan finne og løse problemet.

## Se også
- [Offisiell Go-dokumentasjon om standard error](https://golang.org/pkg/builtin/#func-fprintf)
- [En guide til feilsøking i Go-programmering](https://www.calhoun.io/how-to-debug-go-programs/)
- [Eksempler på å bruke standard error i praksis](https://medium.com/@kennywho/go-error-handling-best-practices-9f7ea26d3b1)