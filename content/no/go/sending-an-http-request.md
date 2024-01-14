---
title:                "Go: Å sende en http forespørsel"
simple_title:         "Å sende en http forespørsel"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler er en viktig del av å bygge moderne webapplikasjoner og kommunisere med andre nettverksbaserte tjenester. Med Go-programmeringsspråket er det enkelt å implementere HTTP-forespørsler og håndtere responsen på en effektiv måte.

## Hvordan

Først må vi importere "net/http" -pakken for å bruke funksjonaliteten for HTTP-forespørsler. Deretter kan vi sende en GET-forespørsel ved å bruke "http.Get()" -funksjonen og oppgi URL-en som en parameter. For å få tilgang til responsen, kan vi bruke "resp.Body" -verdien som returneres, og lese dataene ved hjelp av "ioutil.ReadAll()" -funksjonen. Et eksempel på dette i Go-kode kan se slik ut:

```Go
package main

import(
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    // Send en GET-forespørsel og lagre responsen i en variabel
    resp, err := http.Get("http://example.com")
    if err != nil {
        // Hvis det oppstår en feil, skriv ut den på konsollen
        fmt.Println("Error:", err)
        return
    }
    // Les responsen og skriv ut den som en streng
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    fmt.Println(string(body))
}
```

Dette eksempelet vil skrive ut HTML-koden for nettsiden "http://example.com". Som du kan se, kan vi enkelt få tilgang til responsdataene og behandle dem videre etter behov.

## Dypdykk

For de som er interessert i å lære mer om å sende HTTP-forespørsler i Go, er det noen viktige ting å merke seg.

Først og fremst kan "http.Client" -objektet brukes til å tilpasse innstillinger for en HTTP-forespørsel. For eksempel kan timeout-verdien endres, eller en HTTP-proxy kan legges til for å sende forespørsler gjennom.

I tillegg kan vi bruke "http.NewRequest()" -funksjonen for å lage en HTTP-forespørsel med mer komplekse innstillinger, for eksempel å legge til egendefinerte header-felter eller sende en POST-forespørsel med data.

Det er også viktig å merke seg at Go har innebygde midler for å håndtere feil og gjøre asynkrone HTTP-forespørsler ved hjelp av "gorutiner". Dette gjør det mulig å sende flere forespørsler samtidig uten å måtte vente på at en forespørsel er ferdig før du starter en annen.

## Se også

- [Offisiell dokumentasjon for "net/http" -pakken på Go-språket](https://golang.org/pkg/net/http/)
- [Tutorial om å sende HTTP-forespørsler med Go](https://www.digitalocean.com/community/tutorials/how-to-send-http-requests-in-go)
- [Artikkel om asynkrone HTTP-forespørsler i Go](https://medium.com/@MasimaHeidar/async-http-requests-with-golang-using-goroutines-4226ae9821d9)