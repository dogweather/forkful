---
date: 2024-01-20 17:59:31.597265-07:00
description: 'How to: Fish Shell har ikke innebygd HTTP-funksjonalitet, men du kan
  bruke curl eller httpie.'
lastmod: '2024-03-13T22:44:41.223805-06:00'
model: gpt-4-1106-preview
summary: Fish Shell har ikke innebygd HTTP-funksjonalitet, men du kan bruke curl eller
  httpie.
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## How to:
Fish Shell har ikke innebygd HTTP-funksjonalitet, men du kan bruke curl eller httpie.

```fish
# Med curl, GET-forespørsel
curl https://api.example.com/data

# Output: JSON eller data som serveren svarer med

# Med httpie, GET-forespørsel
http https://api.example.com/data

# Output: Formatert JSON eller data

# Post-forespørsel med ekstra headers og data
curl -X POST https://api.example.com/update \
     -H 'Content-Type: application/json' \
     -d '{"status":"active"}'

# Output: Respons fra serveren etter POST-forespørsel
```

## Deep Dive
Historisk sett har UNIX/Linux-systemer brukt verktøy som curl for nettverksinteraksjoner. Curl og libcurl, utgitt første gang i 1997, er standardverktøyet for å sende HTTP-forespørsler fra kommandolinjen. Httpie, som er nyere, tilbyr enklere syntaks og mer lesbare responser. 

I Fish Shell bruker vi disse verktøyene fordi Shell'en fokuserer på interaktiv bruk og tar ikke sikte på å erstatte dedikert programvare for nettverkshåndtering. Riktig bruk av flagg og parametre er avgjørende for å oppnå ønsket oppførsel fra disse verktøyene.

## See Also
- Curl offisielle dokumentasjon: https://curl.se/docs/
- Httpie GitHub og dokumentasjon: https://github.com/httpie/httpie
- Fish Shell offisiell dokumentasjon: https://fishshell.com/docs/current/index.html
