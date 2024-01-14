---
title:                "Go: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regulære uttrykk (Regex) er et svært nyttig verktøy når man jobber med tekstbaserte data i programmering. Det hjelper deg å finne og manipulere spesifikke mønstre i en tekststreng, og kan spare deg for mye tid og frustrasjon. Det er en essensiell ferdighet å mestre for alle som jobber med data og programmering.

## Hvordan

For å bruke Regex i Go, må du først importere pakken "regexp". Deretter kan du bruke funksjoner som "Compile" og "MatchString" for å lage og matche uttrykk. La oss se på et eksempel:

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Opprett et Regex-uttrykk for å finne alle ord som starter på bokstaven "g"
    reg, _ := regexp.Compile("g\\w+")

    // Lag en liste med tekststrenger for å teste Regex på
    tekst := []string{"go", "golang", "java", "python", "gopher"}

    // Loop gjennom hver tekststreng og sjekk om den matcher uttrykket
    for _, s := range tekst {
        if reg.MatchString(s) {
            fmt.Println(s, "matcher uttrykket")
        } else {
            fmt.Println(s, "matcher ikke uttrykket")
        }
    }
}
```

Dette vil gi følgende output:

```
go matcher uttrykket
golang matcher uttrykket
java matcher ikke uttrykket
python matcher ikke uttrykket
gopher matcher uttrykket
```

Du kan også bruke Regex til å erstatte deler av en tekststreng. I dette tilfellet kan du bruke funksjonen "ReplaceAllString" til å erstatte alle forekomster av et mønster med en annen tekst. Her er et eksempel på hvordan du kan endre alle forekomster av tallet "1" til bokstaven "one":

```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Opprett et Regex-uttrykk for å finne tallet "1"
    reg, _ := regexp.Compile("1")

    // Erstatt alle forekomster av "1" med "one" i teksten
    tekst := reg.ReplaceAllString("Jeg har 1 eple og 12 bananer", "one")

    fmt.Println(tekst)
}
```

Dette vil gi følgende output:

```
Jeg har one eple og 12 bananer
```

Dette var bare et par enkle eksempler på hvordan du kan bruke Regex i Go. Mulighetene er mange og det er bare fantasien som setter grenser.

## Dypdykk

Innenfor Regex finnes det mange forskjellige spesifikke uttrykk og metoder du kan bruke for å finne og manipulere tekst. Det kan være lurt å bruke online verktøy som Regex101 for å teste og eksperimentere med ulike uttrykk før du implementerer dem i kode. Det er også viktig å være klar over at Regex kan være svært ressurskrevende, spesielt hvis du bruker komplekse uttrykk på store datasett. Det er derfor viktig å optimalisere og teste koden din for å unngå at den blir for treg.

## Se også

- [Official Go Regexp Package](https://pkg.go.dev/regexp)
- [Regex101](https://regex101.com/) (online Regex verktøy)
- [Regex Tutorial](https://www.regular-expressions.info/tutorial.html) (guide til Regex)