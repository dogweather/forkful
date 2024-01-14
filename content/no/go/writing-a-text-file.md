---
title:                "Go: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan virke som en enkel og utdatert aktivitet, men i programmering kan det være svært nyttig. En tekstfil kan lagre data som kan være tilgjengelig for din Go-applikasjon når som helst, og kan også brukes til å eksportere data til andre programmer.

## Hvordan

Det første trinnet for å skrive en tekstfil i Go er å opprette en fil ved hjelp av `os.Create ()` funksjonen. Deretter kan du skrive data til filen ved hjelp av `fmt.Fprintf()` funksjonen. La oss se på et eksempel:

```
package main

import (
    "fmt"
    "os"
)

func main() {
    file, err := os.Create("minTekstfil.txt") //Opprettelse av filen "minTekstfil.txt"
    if err != nil {
        fmt.Println(err) //Hvis det oppstår en feil, skriv den ut
        return
    }
    defer file.Close() //Lukk filen når programmet er ferdig
    fmt.Fprintf(file, "Dette er en tekst som vil bli skrevet til filen.") //Skriv tekst til filen
}
```

Når du kjører dette programmet, vil du opprette en ny fil kalt "minTekstfil.txt" med teksten som ble skrevet til den. Du kan endre teksten og prøve å kjøre programmet igjen for å se at den gamle teksten blir overskrevet.

## Dypdykk

For å skrive mer kompleks data til en tekstfil, kan du bruke `bufio` pakken i Go. Denne pakken lar deg lese og skrive data med buffere, noe som kan være mer effektivt. Du kan også bruke `io/ioutil` pakken for å skrive til en tekstfil med mindre kode.

En annen ting å huske på når du jobber med tekstfiler er å håndtere eventuelle feil som kan oppstå. Dette er spesielt viktig når du arbeider med filer som allerede eksisterer, siden de kan være skrivebeskyttet eller utilgjengelige.

## Se også

- Les en tekstfil i Go: https://golang.org/pkg/os/#Stat
- Manipuler data med buffere i Go: https://golang.org/pkg/bufio/
- Skriv til en fil med `io/ioutil` pakken: https://golang.org/pkg/io/ioutil/