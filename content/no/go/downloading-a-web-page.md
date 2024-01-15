---
title:                "Laste ned en nettside"
html_title:           "Go: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor
Du har sikkert vært på nettet og ønsket å laste ned en nettside for offline bruk. Kanskje du vil lese en artikkel på toget, eller bare lagre en interessant side for senere bruk. Uansett, Go gjør det enkelt å laste ned en web side og lagre den på din lokale enhet.

## Hvordan
Det er enkelt å laste ned en web side ved hjelp av Go. Først må du importere "net/http" biblioteket. Deretter kan du bruke "http.Get()" funksjonen for å hente data fra en URL. Så kan du bruke "ioutil.WriteFile()" for å skrive dataene til en fil.

Her er et eksempel på hvordan du laster ned og lagrer en web side:

```Go
package main
import (
    "fmt"
    "net/http"
    "io/ioutil"
)
func main() {
    // Definer URL
    url := "https://www.example.com"
    // Hent data fra URL
    resp, err := http.Get(url)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()
    // Les dataene
    body, err := ioutil.ReadAll(resp.Body)
    // Skriv dataene til en fil
    err = ioutil.WriteFile("eksempel.html", body, 0644)
    // Sjekk for feil
    if err != nil {
        panic(err)
    }
    // Skriv ut suksessmelding
    fmt.Println("Nettsiden er lastet ned og lagret i en fil!")
}
```

Dette vil resultere i en fil som heter "eksempel.html" som inneholder all HTML-koden fra nettsiden.

## Dypdykk
Nå som du har lært å laste ned og lagre en web side, kan du også utforske mer avanserte funksjoner som å endre brukeragent og håndtere redirects. Du kan også bruke "net/http" biblioteket til å gjøre HTTP-anrop og behandle responsen på ulike måter.

## Se også
- [Go's net/http pakke](https://golang.org/pkg/net/http/)
- [Go's ioutil pakke](https://golang.org/pkg/io/ioutil/)
- [HTTP-forespørsler og svar](https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html)