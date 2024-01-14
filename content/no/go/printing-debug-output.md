---
title:    "Go: Utskrift av feilsøkingsutgang"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor
Debugging er en viktig del av enhver programmeringsprosess, og å kunne skrive ut feilmeldinger og annen informasjon for å spore feil er avgjørende. I dette blogginnlegget vil vi utforske hvordan du kan bruke Go-programmeringsspråket til å skrive ut feilmeldinger og debug-informasjon.

## Hvordan
Det første du må gjøre er å importere standardpakken `fmt`. Denne pakken inneholder funksjoner som lar oss skrive ut informasjon til konsollen. Her er et enkelt eksempel på hvordan du kan skrive ut en melding til konsollen:

```Go
package main

import "fmt"

func main() {
    fmt.Println("Hei verden!")
}
```

Dette vil produsere følgende output i konsollen:

```
Hei verden!
```

Du kan også bruke `Printf`-funksjonen for å skrive ut variabler. Her er et eksempel på hvordan du kan skrive ut verdien av en variabel:

```Go
package main

import "fmt"

func main() {
    num := 42
    fmt.Printf("Verdien av num er %d \n", num)
}
```

Dette vil produsere følgende output:

```
Verdien av num er 42
```

For å skrive ut feilmeldinger, kan du bruke `Errorf`-funksjonen. Her er et eksempel på hvordan du kan bruke den:

```Go
package main

import "fmt"

func main() {
    err := fmt.Errorf("Dette er en feilmelding")
    fmt.Println(err)
}
```

Dette vil produsere følgende output:

```
Dette er en feilmelding
```

## Deep Dive
Nå som du har lært hvordan du kan skrive ut feilmeldinger og annen informasjon, kan det være nyttig å vite om noen tilleggsfunksjoner som kan være nyttige under debugging. En av disse er `Sprintf`-funksjonen, som lar deg lagre en formattert streng i en variabel istedenfor å skrive den direkte til konsollen.

En annen nyttig funksjon er `Debugf`, som er spesielt nyttig når du jobber med større prosjekter. Denne funksjonen lar deg skrive ut informasjon til konsollen kun når programmet kjører med en flaggverdi som er satt til å være "debug".

## Se også
- [Offisiell dokumentasjon for fmt-pakken](https://golang.org/pkg/fmt/)
- [Guide til error handling i Go](https://blog.golang.org/error-handling-and-go)
- [Tutorial om debugging i Go](https://www.calhoun.io/5-debugging-tips-for-go-programmers/)