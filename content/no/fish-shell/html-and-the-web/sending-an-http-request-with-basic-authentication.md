---
date: 2024-01-20 18:01:37.645944-07:00
description: "Hvordan gj\xF8re det: HTTP Basic Auth er en autentiseringsmekanisme\
  \ fra WWWs barndom. Alvorlig utdatert i sikkerhetsstandarder, det er enkelt, lite\
  \ staffeli\u2026"
lastmod: '2024-04-05T22:50:55.241251-06:00'
model: gpt-4-1106-preview
summary: HTTP Basic Auth er en autentiseringsmekanisme fra WWWs barndom.
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan gjøre det:
```Fish Shell
set brukernavn "mittBrukernavn"
set passord "mittPassord"
set auth (printf "%s:%s" $brukernavn $passord | base64)

# Send forespørsel med Basic Auth header
curl -H "Authorization: Basic $auth" https://eksempel.com/api/data
```
Eksempelutdata:
```
{
  "respons": "Dataene dine her"
}
```

## Dykk dypere:
HTTP Basic Auth er en autentiseringsmekanisme fra WWWs barndom. Alvorlig utdatert i sikkerhetsstandarder, det er enkelt, lite staffeli og som oftest overgått av sterkere ordninger som OAuth. 

Alternativene til Basic Auth inkluderer Digest Auth, API-nøkler, OAuth, og JWTs (JSON Web Tokens), hvert med sine egne implementasjonsdetaljer og bruksscenarioer. 

Å implementere Basic Auth krever forsiktighet. Base64-koding av legitimasjonene (som Fish Shell eksemplet viser) er ikke kryptering og kan lett dekodes. Sikre alltid kommunikasjonen med HTTPS for å unngå at legitimasjonen blir avlyttet.

## Se også:
- cURL dokumentasjon for http-autentisering: https://curl.se/docs/http-auth.html
- Fish Shell dokumentasjon: https://fishshell.com/docs/current/index.html
- HTTP autentiseringsmetoder oversikt: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Sikkerhetsbetraktninger for Basic Auth: https://owasp.org/www-community/controls/Basic_Authentication
