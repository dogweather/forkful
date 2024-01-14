---
title:                "Haskell: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Sending av HTTP-forespørsler med grunnleggende autentisering er en viktig del av nettverksprogrammering. Dette tillater utveksling av sensitiv informasjon mellom klient og server på en sikker måte.

## Hvordan gjøre det

For å utføre en HTTP-forespørsel med grunnleggende autentisering i Haskell, kan vi bruke biblioteket `http-conduit`. Først må vi importere biblioteket og noen av dets funksjoner:

````Haskell
import Network.HTTP.Conduit

res <- do
    request <- parseUrlThrow "URL-adresse"
    manager <- newManager tlsManagerSettings
    let request' = applyBasicAuth "brukernavn" "passord" request
    httpLbs request' manager
````

Her bruker vi `parseUrlThrow` for å lage en `Request` fra en URL-adresse. Deretter lager vi en `Manager` ved hjelp av standardinnstillingene i `tlsManagerSettings`. `applyBasicAuth` funksjonen legger til grunnleggende autentisering til vår opprinnelige forespørsel. Til slutt sender vi vår forespørsel ved hjelp av `httpLbs` og mottar et svar fra serveren.

For å få tilgang til svaret, kan vi bruke `responseBody` funksjonen sammen med `res` verdi fra koden ovenfor.

````Haskell
let responseBody' = responseBody res
````

Svaret vil være i `ByteString` format, som vi kan konvertere til en `String` ved hjelp av `Data.ByteString.Char8` modulen.

## Dypdykk

HTTP-forespørsler med grunnleggende autentisering krever at brukernavn og passord blir sendt som en del av forespørselen i klartekst. Dette er en av de enkleste formene for autentisering, men den er ikke så sikker som andre autentiseringstyper som f.eks. OAuth. Det er viktig å sørge for at nettverkskommunikasjonen er sikret ved hjelp av TLS (Transport Layer Security) eller SSL (Secure Sockets Layer) når du bruker grunnleggende autentisering.

Det er også viktig å merke seg at grunnleggende autentisering ikke krypterer brukernavn eller passord, så det er mulig for noen å fange opp denne informasjonen og få tilgang til kontoen din. Derfor er det viktig å bruke sterkere autentiseringstyper hvis det er mulig.

## Se også

- [Haskell HTTP Client dokumentasjon](https://hackage.haskell.org/package/http-client)
- [Intro to Network Conduit tutorial](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/http-conduit)
- [TLS Manager Documentation](https://hackage.haskell.org/package/http-client-tls/docs/Network-HTTP-Client-TLS.html)