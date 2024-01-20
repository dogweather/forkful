---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering betyr å sende en nettforespørsel med brukernavn og passord. Programmerere gjør det for å sikre datautveksling og begrense tilgang til spesifikke ressurser.

## Hvordan:

Du kan bruke cURL-verktøyet til å sende HTTP-forespørsler fra Bash. Husk å erstatte `brukernavn`, `passord` og `url` med riktige verdier.

```Bash
#!/bin/bash

brukernavn="brukernavn"
passord="passord"
url="http://eksempel.com"

curl -u $brukernavn:$passord $url
```

Når du kjører dette skriptet, vil svaret fra serveren vises i terminalen.

## Dypere innblikk

Grunnleggende autentisering i HTTP ble introdusert i 1996 med HTTP/1.0. Selv om det ikke er det sikreste alternativet, brukes det fremdeles i noen eldre eller enkle systemer. Sikrere alternativer inkluderer token-basert autentisering og OAuth. 

Når du sender en forespørsel med grunnleggende autentisering, legges brukernavnet og passordet til i en overskrift i forespørselen. Dette blir kodet til Base64, men det er viktig å merke seg at Base64 ikke er en kryptering, og det kan dekodes lett.

## Se også

1. [HTTP Authentications](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
3. [cURL Manual](https://curl.se/docs/manual.html)
4. [Using cURL for Remote Requests](https://linuxize.com/post/curl-command-examples/)
5. [cURL vs HTTPie](https://www.ateam-oracle.com/httpie-vs-curl)