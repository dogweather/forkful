---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering innebærer at forespørselen inneholder brukernavn og passord i "Authorization" overskriften. Programmerere gjør dette for å autorisere tilgang til bestemte API-enderpunkter.

## Slik gjør du: 
For å sende en HTTP-forespørsel med grunnleggende autentisering i Fish Shell, bruk curl-kommandoen. La oss vise med et eksempel:

```Fish Shell
set user brukernavn
set pass passord
curl -u $user:$pass https://eksempel.com/api
```

Curl utføre forespørselen, og serverens svar vil bli skrevet til terminalen.

## Dyp Dykk
Historisk sett, HTTP Basic Authentication ble foreslått i 1999 som del av HTTP 1.1 standarden. Til tross for sin alder, er den fortsatt i bred bruk på grunn av sin enkelhet.

Alternativer til Basic Authentication inkluderer Digest Authentication, OAuth, eller bruk av API nøkler. Disse tilbyr forskjellige nivåer av sikkerhet og kompleksitet, avhengig av dine behov.

Implementeringen av HTTP Basic Authentication i Fish Shell benytter seg av curl, et kommandolinjeverktøy som gjør det mulig å utføre alle typer HTTP-forespørsler.

## Se Også
For ytterligere lesing om HTTP-forespørsler og Basic Authentication, sjekk ut disse lenkene:

- [Mozilla Developer Network: HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [cURL dokumentasjon](https://curl.haxx.se/)
- [Wikipedia: Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)