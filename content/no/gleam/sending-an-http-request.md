---
title:                "Gleam: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende en HTTP-forespørsel er en viktig del av moderne programvareutvikling. Det lar deg kommunisere med andre programmer og tjenester på nettet og åpner dørene for å bygge kraftige og dynamiske applikasjoner.

## Hvordan

For å sende en HTTP-forespørsel i Gleam, kan du bruke biblioteket "httpc" som er inkludert som standard i språket. Først må du importere biblioteket ved å legge til følgende linje øverst i filen:

```Gleam
import httpc
```

Deretter kan du opprette en forespørsel og velge hvilken metode du vil bruke. Her er et eksempel på å sende en GET-forespørsel til en nettside:

```Gleam
let request = httpc.get("https://www.example.com")
```

Du kan også legge til eventuelle parametere eller kropp til forespørselen din:

```Gleam
let body = httpc.Body.text("Dette er en eksempeltekst")
let params = [("page", "2"), ("sort", "date")]
let request = httpc.post("https://www.example.com/articles", body, params)
```

Når du har opprettet forespørselen, kan du sende den ved å bruke funksjonen "send" og lagre svaret i en variabel:

```Gleam
let response = httpc.send(request)
```

Du kan deretter få tilgang til informasjonen i svaret, for eksempel statuskoden og innholdet i responskroppen:

```Gleam
let status = httpc.Response.status(response)
let body = httpc.Response.body(response)
```

## Dypdykk

Når du sender en HTTP-forespørsel, er det viktig å håndtere eventuelle feil som kan oppstå. I Gleam kan du bruke "try" og "match"-uttrykk for å håndtere disse feilene på en elegant måte. For eksempel:

```Gleam
let response = 
  try match httpc.send(request) {
    Ok(r) -> r
    Error(e) -> httpc.Response.error_response(e)
  }
```

Du kan også spesifisere hvilken type respons du forventer å få ved å bruke "body_as" -funksjonen. For eksempel, hvis du forventer en HTML-respons, kan du skrive følgende:

```Gleam
let html_body = httpc.Response.body_as(response, httpc.Body.html)
```

Hvis du trenger mer kontroll over forespørselen din, kan du også bruke "request" -funksjonen til å manuelt sette headere og andre parametere før du sender den.

## Se Også

- [Gleam HTTP-bibliotek dokumentasjon](https://gleam.run/modules/httpc)
- [Din første Gleam HTTP GET-forespørsel](https://medium.com/luizalabs/din-f%C3%B8rste-gleam-http-get-foresp%C3%B8rsel-a0a59b9dad7c)
- [Bygg en API-klient i Gleam](https://medium.com/luizalabs/build-an-api-client-in-gleam-54ea971c8a86)