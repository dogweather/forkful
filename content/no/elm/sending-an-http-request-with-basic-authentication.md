---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Elm: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP forespørsel med basic authentication er en måte å sikre at bare autoriserte brukere får tilgang til en ressurs. Dette gjøres ved å inkludere et brukernavn og passord i forespørselen, som deretter blir validert av serveren. Programmere benytter seg av dette for å sikre at bare tillitsskapte brukere kan få tilgang til sensitiv informasjon eller funksjonalitet på et nettsted eller applikasjon.

## Hvordan gjøre det:
Sending av HTTP-forespørsler med basic authentication kan gjøres enkelt i Elm ved hjelp av HTTP-pakken. Her er et eksempel på hvordan en slik forespørsel kan se ut:

```Elm
Http.toRequest
    { method = "GET"
    , headers =
      [ ( "Authorization", "Basic <base64 encoded user:password>" )
      ]
    , url = "https://example.com/api/endpoint"
    , body = Http.emptyBody
    }
```

En HTTP-forespørsel med basic authentication inneholder altså en header med brukernavn og passord som er kodet ved hjelp av Base64. Dette sørger for at det er vanskeligere å dekode og at informasjonen ikke kan leses av uvedkommende.

## Dykk dypere:
Basic authentication har eksistert siden tidlig på 90-tallet og er en av de mest brukte autentiseringsmetodene på nettet. Alternativer inkluderer for eksempel Digest authentication og OAuth 2.0. Implementeringsdetaljer kan variere mellom ulike servere og plattformer, men prinsippet er det samme – å inkludere brukernavn og passord i en HTTP-forespørsel for å validere brukerens identitet og gi tilgang til ressursen.

## Se også:
- The Elm HTTP package documentation: https://package.elm-lang.org/packages/elm/http/latest/
- Basic authentication explained: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme