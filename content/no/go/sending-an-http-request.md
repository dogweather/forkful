---
title:                "Sending en http-forespørsel"
html_title:           "Go: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler er en essensiell del av webutvikling, og med Go's innebygde nettpakker er det enkelt å implementere dette i programmene dine. Ved å sende HTTP-forespørsler, kan du hente data fra eksterne API-er, kommunisere med servere og mye mer.

## Hvordan

Først må du importere "net/http" pakken i koden din. Deretter kan du bruke "http.Get()" funksjonen for å sende en HTTP-forespørsel til en angitt URL. For å få tilgang til svaret og håndtere eventuelle feil, kan du bruke "resp, err := http.Get()" og deretter bruke "resp.Body" til å lese svaret fra nettstedet.

```Go
package main

import (
  "fmt"
  "net/http"
)

func main() {
  resp, err := http.Get("https://example.com")

  if err != nil {
    panic(err)
  }
  
  fmt.Println("Statuskode:", resp.StatusCode)
}
```

I dette eksempelet sender vi en HTTP GET-forespørsel til nettsiden "https://example.com" og skriver ut svaret ved å få tilgang til "StatusCode" -attributtet i "resp" -variabelen. Dette er en enkel måte å sjekke om forespørselen vår var vellykket eller ikke.

## Dypdykk

Det er mange ulike måter å tilpasse og forbedre dine HTTP-forespørsler. Du kan legge til "Headers" for å sende spesifisert informasjon til nettstedet, som autentiseringsnøkler eller user-agent. Du kan også bruke ulike metoder som "POST" eller "PUT" for å sende data til serveren i tillegg til å bare få data fra den.

I tillegg lar Go deg også bygge egne HTTP-handlere, som kan brukes til å opprette din egen server og svare på inngående forespørsler. Dette gir deg enda mer kontroll og fleksibilitet til å utvikle dine egne webapplikasjoner.

## Se også

- [Nett-pakker i Go](https://golang.org/pkg/net/)
- [Offisiell Go-dokumentasjon for å håndtere HTTP-forespørsler](https://golang.org/doc/tutorial/web-service-gin)
- [Eksempelkode for å sende HTTP-forespørsler i Go](https://github.com/golang/go/wiki/Draft-Go-HTTP-clients)