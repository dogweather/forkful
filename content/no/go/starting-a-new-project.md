---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starte et nytt prosjekt i Go Programming Language

Velkommen til en lettfattelig og rett på sak innføring i Go programmering.

## Hva & Hvorfor?
Starte et nytt prosjekt kan sees på som å legge grunnsteinen for et nytt bygg. Programmerere gjør dette for å strukturere og organisere kode på en effektiv og gjenbrukbar måte.

## Hvordan:
Her er et typisk "Hallo Verden!"-eksempel i Go. Å starte et nytt Go-prosjekt er så enkelt som å skrive denne koden:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hallo Verden!")
}
```

Når du kjører denne koden, vil outputet i kommandolinjen din være:

```
Hallo Verden!
```

## Dypdykk
1. **Historisk kontekst:** Go ble skapt i 2007 av Google for å forbedre programmeringseffektiviteten. Et viktig aspekt ved oppstart av et Go-prosjekt er organisering av kode i pakker for å lette vedlikehold, distribusjon, og gjenbruk.
2. **Alternativer:** Andre språk som Java, Python, eller Ruby tilbyr lignende prosjektstruktur funksjoner. Valget avhenger av dine preferanser og behov.
3. **Implementeringsdetaljer:** Når du starter et nytt Go-prosjekt, lag en Hovedmappe (Root), src-mappe (source), og en bin-mappe (binær). Go bruker `$GOPATH` for å identifisere hvor dine Go filer ligger. 

## Se Også
- [Tour of Go](https://tour.golang.org/welcome/1): En interaktiv tur for å lære Go
- [The Go Programming Language Dokumentasjon](https://golang.org/doc/): For deg som ønsker å dykke dypere
- [Effective Go](https://golang.org/doc/effective_go): Utdypende Go tips og teknikker. 

Å starte et nytt prosjekt kan virke overveldende, men husk at hvert stort prosjekt begynner med en liten linje kode. Lykke til!