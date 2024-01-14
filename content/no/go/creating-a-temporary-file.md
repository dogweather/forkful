---
title:    "Go: Lage en midlertidig fil"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Hvorfor

Det å opprette midlertidige filer kan være nyttig når du trenger å lagre data i løpet av en programkjøring eller når du trenger å kommunisere med andre programmer gjennom delte filer.

## Slik gjør du det

Her er et eksempel på hvordan du kan opprette en midlertidig fil i Go-programmeringsspråket:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // Oppretter en midlertidig fil med navnet "tempfile" i systemets standard midlertidige mappe
    f, err := ioutil.TempFile("", "tempfile")

    if err != nil {
        fmt.Println(err)
        return
    }

    defer os.Remove(f.Name()) // Sletter den midlertidige filen når programmet er ferdig

    fmt.Println("Midlertidig fil opprettet på følgende bane:", f.Name())
}
```

Eksempelutgang:

```
Midlertidig fil opprettet på følgende bane: /tmp/tempfile714814842
```

Det følgende kodelinjen sletter den midlertidige filen når programmet er ferdig for å unngå å etterlate unødvendige filer i systemet.

```Go
defer os.Remove(f.Name())
```

Det finnes flere måter å opprette midlertidige filer på i Go, for eksempel ved hjelp av `ioutil.TempDir()` eller `ioutil.TempFile()`. Konsulter [Go-dokumentasjonen](https://golang.org/pkg/io/ioutil/#TempFile) for å lære mer om hvordan du kan bruke disse funksjonene for å opprette midlertidige filer.

## Dykk dypere

Når du oppretter en midlertidig fil, blir den plassert i systemets standard midlertidige mappe. Du kan også spesifisere en annen mappe eller bane for å lagre den midlertidige filen ved å angi en bane som første argument i `ioutil.TempFile()` funksjonen.

Det kan også være nyttig å vite at du kan bruke `f.Name()` funksjonen for å få navnet på den midlertidige filen som ble opprettet. Dette kan være nyttig hvis du trenger å lese, skrive eller slette den midlertidige filen senere i programmet.

## Se også

- [Go-dokumentasjonen om midlertidige filer](https://golang.org/pkg/io/ioutil/#TempFile)
- [Guide til Go-programmeringsspråket på norsk](https://www.learngoprogramming.com/go/)