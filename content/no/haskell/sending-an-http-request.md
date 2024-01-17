---
title:                "Å sende en http-forespørsel"
html_title:           "Haskell: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

### Hva & Hvorfor?

Når vi lager programmer, kan vi noen ganger trenge å kommunisere med andre tjenester eller nettsteder. Dette kan vi gjøre ved å sende en HTTP forespørsel. Det er en måte å be om informasjon fra en annen tjeneste på, og det er noe som programmerere gjør for å få tilgang til data og integrere med andre systemer.

### Hvordan å:

For å sende en HTTP forespørsel i Haskell, kan vi bruke biblioteket "http-client". Først må vi importere biblioteket og definere en funksjon som vil utføre vår forespørsel. Her er et eksempel på en funksjon som sender en GET forespørsel til en nettside og skriver ut svaret til konsollen:

```Haskell
import Network.HTTP.Client

performRequest :: IO ()
performRequest = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "https://www.example.com"
  response <- httpLbs request manager
  print (responseBody response)
```
Dette er en veldig enkel måte å sende en HTTP forespørsel på, og det finnes flere måter å gjøre det på avhengig av hva slags informasjon du ønsker å sende og få tilbake. 

### Dypdykk:

HTTP er et protokoll som ble utviklet på 1990-tallet for å la klienter og servere kommunisere på et nettverk. Det finnes også andre protokoller som kan brukes for å kommunisere over nettverk, som for eksempel FTP og SMTP. I dag er HTTP den vanligste protokollen som brukes på verdensveven.

Et alternativ til å bruke biblioteket "http-client" er å bruke "wreq", som også er et populært Haskell bibliotek for å sende HTTP forespørsler. Dette biblioteket har en mer intuitiv og minimalistisk syntaks, men begge bibliotekene fungerer godt for å sende og behandle HTTP forespørsler.

Når du sender en HTTP forespørsel i Haskell, vil funksjonen "performRequest" som vi definerte i "## How to:" seksjonen, utføre flere sekvensielle operasjoner bak kulissene. Dette inkluderer å åpne en TCP forbindelse, sende forespørsel og motta respons. Det er viktig å være oppmerksom på disse operasjonene når du sender en forespørsel fordi de kan påvirke ytelsen til applikasjonen din.

### Se også:

Haskell "http-client" dokumentasjon: https://hackage.haskell.org/package/http-client
Haskell "wreq" dokumentasjon: https://hackage.haskell.org/package/wreq