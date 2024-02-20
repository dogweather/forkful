---
date: 2024-01-20 18:02:00.092330-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering inneb\xE6\
  rer \xE5 inkludere brukernavn og passord for \xE5 f\xE5 tilgang til beskyttede ressurser.\u2026"
lastmod: 2024-02-19 22:05:00.105556
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering inneb\xE6\
  rer \xE5 inkludere brukernavn og passord for \xE5 f\xE5 tilgang til beskyttede ressurser.\u2026"
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering innebærer å inkludere brukernavn og passord for å få tilgang til beskyttede ressurser. Programmører gjør dette for å kommunisere sikkert med webtjenester som krever identifikasjon.

## Slik Gjør Du:
For å gjøre en HTTP-forespørsel med grunnleggende autentisering i Haskell, kan du bruke `http-client` og `http-client-tls` bibliotekene. Her er et eksempel:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hAuthorization)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  let creds = "brukernavn:passord"
      url = "https://eksempel.no/minside"
      headers = [(hAuthorization, "Basic " <> encode (BS.pack creds))]

  initialRequest <- parseRequest url
  let request = initialRequest { method = "GET", requestHeaders = headers }

  response <- httpLbs request manager
  putStrLn $ "Statuskode: " ++ show (responseStatus response)
  BS.putStrLn $ responseBody response
```

Dette sender en GET-forespørsel til en URL med grunnleggende autentisering. `responseStatus` og `responseBody` gir statuskoden og svaret.

## Dypdykk
Grunnleggende autentisering i HTTP er en metode for en HTTP-brukeragent (som en webleser) til å gi brukernavn og passord ved forespørsler. Den har eksistert siden de tidligere dagene av HTTP og regnes for å være enkel, men ikke den sikreste metoden for autentisering.

Alternativer for grunnleggende autentisering inkluderer OAuth, API-nøkler og tokens. Disse metodene gir sterkere sikkerhet og er mer velegnet for moderne applikasjoner.

Implementeringsdetaljer for HTTP-forespørsler med grunnleggende autentisering i Haskell fokuserer på å riktig kode brukernavn og passord med Base64, og sørge for at HTTPS brukes for å beskytte legitimasjonsbevisene i transitt.

## Se Også
- Haskell `http-client` pakke: https://www.stackage.org/package/http-client
- `http-client-tls` for sikre forbindelser: https://www.stackage.org/package/http-client-tls
- HTTP autentisering på MDN: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Sikkerhetsaspekter ved HTTP Basic Auth: https://owasp.org/www-community/controls/Basic_Authentication
- Alternative autentiseringsmetoder: https://auth0.com/docs/authentication
