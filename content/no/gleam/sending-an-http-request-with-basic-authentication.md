---
title:                "Gleam: Å sende en http forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http forespørsel med grunnleggende autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du trenger å kommunisere med et API som krever godkjenning, er det nødvendig å sende en HTTP forespørsel med grunnleggende autentisering. Dette sikrer at bare autoriserte brukere kan få tilgang til API-en og beskytter sensitiv informasjon.

## Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering i Gleam, må du først importere http- og unicode-modulene. Deretter kan du bruke ```Gleam.Http.request``` funksjonen og spesifisere hvilken metode (GET, POST, PUT, etc.) du trenger, samt URL-en til API-en du kommuniserer med. I koden nedenfor, bruker vi ```Gleam.Unicode.utf8_to_binary``` funksjonen for å konvertere brukernavnet og passordet til hex-verdier som kreves for grunnleggende autentisering.

```
import http
import unicode

let username = "bruker"
let password = "passord"

let url = "https://api.com/"

let headers =
  List.map(Tuple2.new,
    ["Authorization"("Basic "
         ++ Gleam.Unicode.utf8_to_binary(username ++ ":" ++ password))
    ])

let response = Gleam.Http.request(
  method: "GET",
  url: url,
  headers: headers
)
```

Eksempel på output (dersom forespørselen var vellykket):

```
status_code: 200,
headers: [...],
body: "..."
```

## Dypdykk

En HTTP-forespørsel med grunnleggende autentisering inneholder en "Authentication" header, som i koden over er satt til "Basic" etterfulgt av en hex-verdi av brukernavn og passord kombinert med et kolon. Dette sikrer at dataene blir sendt i kryptert form og ikke kan leses av uautoriserte.

Når du sender en HTTP-forespørsel med Gleam, blir dataene automatisk convertert til binærform før de sendes. Dette sikrer at dataene er korrekt formatert for kryptering og senderingen.

## Se Også

https://gleam.run/modules/http.html
https://gleam.run/modules/unicode.html
https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication