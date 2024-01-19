---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP forespørsel er prosessen med å sende en melding fra en klient til en server over internett. Dette gjør programmerere for å hente eller sende data, som er essensielt i moderne webutvikling.

## Hvordan til:
Vi skal bruke biblioteket `http-conduit` for å sende HTTP-forespørsler. Først må vi installere det:

```Haskell
cabal install http-conduit
```

Nå kan vi lage en enkel GET-forespørsel:

```Haskell
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)

main = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "http://httpbin.org/get"
  response <- httpLbs request manager

  liftIO $ print (responseStatus response)
  liftIO $ print (responseBody response)
```

Når du kjører koden, vil du se noe liknende:

```Haskell
Status {statusCode = 200, statusMessage = "OK"}
"{\"args\":{}, ... }"
```

Betydningen er at vi har fått en vellykket respons (200 = OK) fra serveren.

## Dypdykk
Sending av HTTP-forespørsler kommer fra behovet for kommunikasjon mellom klient-server-maskiner, en arkitektur som stammer helt tilbake til tidlig utvikling av nettverk i 1970-årene.

Alternativene til `http-conduit` inkluderer `http-client`, som også gir lavnivå tilgang til HTTP-operasjoner, og mer spesialiserte biblioteker som `wreq` og `req`, som gir mer abstraksjon og brukervennlighet på bekostning av kontroll.

Når du sender en HTTP-forespørsel i Haskell, blir konseptene funksjonell programmering tydelige: hver handling du tar returnerer en ny "ting", i motsetning til å endre en eksisterende ting i stedet. Denne immutabiliteten gir forutsigbarhet og lettere feilfraing.

## Se også
[HaskellWiki HTTP](https://wiki.haskell.org/HTTP) - For mer komplekse eksempler og brukstilfeller av HTTP i Haskell.<br>
[http-conduit på Hackage](https://hackage.haskell.org/package/http-conduit) - For dokumentasjon og mer informasjon om biblioteket vi brukte i dette eksemplet.