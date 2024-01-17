---
title:                "Send en http-forespørsel med grunnleggende autentisering"
html_title:           "Bash: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sending av HTTP-forespørsler med grunnleggende autentisering er en måte for programmere å kommunisere med en ekstern server eller API ved å sende innloggingsinformasjon med forespørselen. Programmerere gjør dette for å få tilgang til beskyttede ressurser på en sikker måte og utføre ulike oppgaver som krever autentisering.

## Hvordan:
For å sende en HTTP-forespørsel med grunnleggende autentisering, bruker du følgende kode i en Bash-fil:

```Bash
# Sett variabler for brukernavn og passord
brukernavn="brukernavn"
passord="passord"

# Kall på netcat for å sende forespørselen og inkluder autentiseringsinformasjon
echo "GET /beskyttetressurs HTTP/1.1\r\nAuthorization: Basic $(echo -n "$brukernavn:$passord" | base64)\r\nHost: eksternserver.com\r\n\r\n" | nc eksternserver.com 80
```

Dette vil lage en HTTP-forespørsel med GET-metoden for å få tilgang til en beskyttet ressurs på en ekstern server. Autentiseringsinformasjonen blir sendt som en del av forespørselen, og du vil motta svar fra serveren i form av HTTP-header og kropp.

## Dypdykk:
Sending av HTTP-forespørsler med grunnleggende autentisering har blitt brukt i mange år, men er fortsatt en vanlig metode for autentisering i dag. Alternativer til grunnleggende autentisering inkluderer mer sikre autentiseringsprotokoller som HMAC (hash-based message authentication code) og OAuth (åpen standard for klientautentisering). Implementeringen av grunnleggende autentisering i Bash kan også være sårbar for sikkerhetshull, så det anbefales å bruke mer avanserte metoder for autentisering når det er mulig.

## Se også:
- [How Basic Authentication Works](https://www.plainenglish.io/basic-authentication-explained-e48465abe5d8)
- [Secure Your API with Basic Authentication](https://blog.jscrambler.com/secure-your-api-with-basic-authentication/)