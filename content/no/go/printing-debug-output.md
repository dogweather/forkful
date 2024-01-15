---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Go: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Du har kanskje lurt på hvorfor man bør bruke debugging-utskrift når man koder i Go. Svaret er ganske enkelt - det kan hjelpe deg med å identifisere eventuelle feil eller problemer i koden din. Å ha god debugging-praksis kan gjøre det enklere å finne og fikse feil, spesielt når du jobber med større og mer komplekse kodeprosjekter.

## Hvordan

For å printe debugging-utskrift i Go, kan du bruke funksjonen `fmt.Printf()`. Denne funksjonen tar inn en formateringsstreng og variabler som skal være med i utskriften, og printer dem ut i terminalen.

```Go
package main

import "fmt"

func main() {
    x := 42
    y := "Hello"

    fmt.Printf("Denne variabelen har verdien %d, mens denne har verdien %s", x, y)
}
```

Output av koden over vil være følgende:

```
Denne variabelen har verdien 42, mens denne har verdien Hello
```

Du kan også bruke `fmt.Println()` hvis du bare ønsker å printe en enkel linje med tekst eller en variabel uten formatering.

```Go
package main

import "fmt"

func main() {
    z := true

    fmt.Println("Denne variabelen er sann: ", z)
}
```

Output av koden over vil være: 

```
Denne variabelen er sann: true
```

Når du printer ut i Go, kan du også bruke verbene `Sprintf()` og `Fprintf()` for å formatere strenger og printe dem til en variabel eller fil. Dette kan være nyttig hvis du vil logge utskrift til en fil i stedet for terminalen.

## Deep Dive

Det finnes flere måter å bruke `fmt.Printf()` på, for eksempel å spesifisere antall desimaler ved å bruke `%f` for flyttall, eller å printe ut boolske verdier som Ja/Nei istedenfor true/false ved å bruke `%t`. Du kan også bruke `%v` for å printe en variabel uten å spesifisere datatypen.

I tillegg til `fmt`-pakken, kan du også forskjellige debugging-verktøy som Visual Studio Code debugger for å få ytterligere informasjon om koden din og feilsøke mer komplekse problemer.

## Se også

- [Offisiell Go-dokumentasjon om debugging](https://golang.org/doc/gdb)
- [Visual Studio Code debugging i Go](https://code.visualstudio.com/docs/languages/go#_debugging)
- [Debugging-tips for Go](https://blog.golang.org/debugging-techniques)