---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Haskell: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har brukt en nettleser til å logge inn på en nettside, har du mest sannsynlig brukt basic authentication. Dette er en enkel måte å sende en HTTP-forespørsel med brukernavn og passord for å autentisere deg mot en server. I denne artikkelen skal vi se på hvordan man kan gjøre dette i Haskell.

## Kom i gang

For å sende en HTTP-forespørsel med basic authentication i Haskell, trenger vi å bruke pakken "Network.HTTP.Simple". Først må vi importere pakken og definere variabler for brukernavn og passord:

```Haskell
import Network.HTTP.Simple

username = "brukernavn"
password = "passord"
```

Deretter bruker vi funksjonen "setRequestBasicAuth" for å legge til autentisering i vår HTTP-forespørsel:

```Haskell
request <- setRequestBasicAuth username password $ parseRequest_ "http://www.example.com"
```

Vi bruker også funksjonen "parseRequest_" for å konvertere en streng til en HTTP-forespørsel. Vi kan deretter sende forespørselen og håndtere svaret på følgende måte:

```Haskell
response <- httpJSON request
print $ getResponseBody response
```

Denne koden vil sende en HTTP GET-forespørsel til "http://www.example.com" med basic authentication og skrive ut svaret fra serveren som en JSON-verdi.

## Dykk dypere

Når vi bruker basic authentication, sender vi brukernavn og passord i klartekst i HTTP-forespørselen. Dette kan være en sikkerhetsrisiko, spesielt hvis man sender følsom informasjon. Det er derfor viktig å bruke HTTPS for å kryptere kommunikasjonen mellom klient og server.

Det finnes også alternative metoder for autentisering, som for eksempel "digest authentication" som bruker en hashfunksjon for å sikre passordet. Dette er mer sikkert enn basic authentication, men krever mer kompleksitet i koden.

## Se også

- [Network.HTTP.Simple dokumentasjon](http://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html)
- [En guide til basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [En sammenligning av autentiseringsmetoder i HTTP](https://www.sciencedirect.com/science/article/pii/B9780128008928000024)