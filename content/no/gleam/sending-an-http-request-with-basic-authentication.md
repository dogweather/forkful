---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Gleam: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når du sender en HTTP-forespørsel med grunnleggende autentisering, betyr det at du sender en forespørsel til et nettsted ved hjelp av et brukernavn og passord for å bekrefte identiteten din. Dette er en vanlig måte for programmerere å autentisere sine forespørsler og få tilgang til sikre nettsider.

## Hvordan:
```Gleam
let request = http.request(
  method="GET",
  headers=[
    "Authorization": "Basic YWxleGRhdGU6cGFzc3dvcmQ=",
  ],
  url="https://www.example.com/api/data"
)

let response = http.send(request)
```
Output:
```
Status Code: 200
Body: {"data": "some data"}
```

## Dypdykk:
Grunnleggende autentisering er en del av HTTP-protokollen og har blitt brukt i mange år som en sikkerhetsmetode for å beskytte nettverkstrafikken. Det finnes også andre autentiseringsmetoder som OAuth og API-nøkler, men grunnleggende autentisering er fortsatt et populært valg for enkelhet og pålitelighet. Implementeringen av dette i Gleam er enkel og kan være nyttig for å få tilgang til tjenester som krever brukernavn og passord.

## Se også:
- [HTTP-protokollen](https://developer.mozilla.org/nb/docs/Web/HTTP)
- [OAuth-autentisering](https://oauth.net/2/)
- [Gleam HTTP-biblioteket](https://gleam.run/libraries/http/)