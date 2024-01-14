---
title:                "Haskell: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside kan være nyttig for å lese en artikkel offline, å analysere data eller å automatisere en oppgave. Haskell har et enkelt og elegant bibliotek for å gjøre dette, noe som gjør det til et godt valg for denne oppgaven.

## Hvordan gjøre det

For å laste ned en nettside i Haskell, må du først importere "Network.HTTP.Simple" biblioteket. Deretter kan du bruke funksjonen "httpBS" for å gjøre en HTTP-forespørsel.

```Haskell
import Network.HTTP.Simple

request <- parseRequest "https://example.com"
response <- httpBS request

putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
putStrLn $ "Headers: " ++ show (getResponseHeaders response)
putStrLn $ "Body: " ++ show (getResponseBody response)
```

Dette vil skrive ut statuskoden, overskriftene og kroppen til nettsiden. Du kan også endre HTTP-metoden, legge til parametere og sette tilpassede overskrifter ved hjelp av "setRequestMethod", "setRequestBody" og "setRequestHeaders" funksjoner.

## Dypdykk

Internett er en kompleks verden, og det er mye mer å laste ned enn bare tekst og bilder. Med "Network.HTTP.Simple" biblioteket, kan du også laste ned og behandle JSON data ved å bruke "httpJSON" funksjonen.

```Haskell
import Network.HTTP.Simple
import Data.Aeson (decode)

request <- parseRequest "https://example.com/api"
response <- httpBS request

let json = decode $ getResponseBody response
case json of
    Nothing -> putStrLn "Invalid JSON"
    Just x -> print x
```

Du kan også behandle feil og håndtere cookies ved å bruke "httpLBS" funksjonen.

## Se også

- https://hackage.haskell.org/package/http-client - Offisiell dokumentasjon for "Network.HTTP.Simple" biblioteket.
- https://hackage.haskell.org/package/aeson - Dokumentasjon for "Data.Aeson" biblioteket for å arbeide med JSON-data.
- https://mail.haskell.org/pipermail/haskell-cafe/2011-September/095265.html - Diskusjon om forskjellen mellom "httpBS" og "httpLBS" funksjoner.