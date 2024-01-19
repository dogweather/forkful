---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel innebærer å kommunisere med en server over internett ved bruk av HTTP (Hypertext Transfer Protocol). Programmerere gjør dette for å hente eller sende data til/fra en server, som når de bruker en API.

## Hvordan:

Her er en grunnleggende HTTP GET-forespørsel i Go:
```Go
package main

import (
  "io/ioutil"
  "net/http"
  "log"
)

func main() {
  resp, err := http.Get("http://example.com/")
  if err != nil {
    log.Fatalln(err)
  }

  body, err := ioutil.ReadAll(resp.Body)
  if err != nil {
    log.Fatalln(err)
  }

  log.Println(string(body))
}
```
Utførelse av dette programmet vil føre til at innholdet på nettsiden "http://example.com/" blir skrevet ut til konsollen.

## Dypdykk:

HTTP ble utformet i 1991 og har vært ryggraden i dataoverføring på internett siden da. Alternativer til HTTP inkluderer andre overføringsprotokoller som FTP eller moderne protokoller som gRPC, men HTTP forblir mest brukt.

I Go må du være oppmerksom på at en vellykket http.Get()-forespørsel ikke nødvendigvis betyr at serveren returnerte en 200 OK-status. Du må manuelt sjekke StatusCode-feltet i resp-objektet.

Du har også mulighet til å tilpasse HTTP-forespørsler ved å bruke http.NewRequest() og http.Client.Do(), hvor du kan tilpasse detaljer som HTTP-metode, overskrifter og timeout-oppførsel.

## Se Også:

Her er noen lenker til relatert informasjon og ressurser for videre læring:

- Go dokumentasjonen på net/http pakken: https://golang.org/pkg/net/http/
- En mer detaljert guide til net/http pakka: https://gobyexample.com/http-clients
- HTTP/2 i Go: https://blog.golang.org/http2
- HTTP.StatusCodes forklaring: http://golang.org/pkg/net/http/#pkg-constants