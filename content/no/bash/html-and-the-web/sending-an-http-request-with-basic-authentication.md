---
date: 2024-01-20 18:01:06.420449-07:00
description: "Slik gj\xF8r du: \xC5 sende en HTTP-foresp\xF8rsel med grunnleggende\
  \ autentisering i Bash kan gj\xF8res enkelt med `curl`. Her er et eksempel."
lastmod: '2024-03-13T22:44:40.974540-06:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering i Bash\
  \ kan gj\xF8res enkelt med `curl`."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Slik gjør du:
Å sende en HTTP-forespørsel med grunnleggende autentisering i Bash kan gjøres enkelt med `curl`. Her er et eksempel:

```Bash
#!/bin/bash

# Definer brukernavn og passord
brukernavn="minBruker"
passord="mittPassord"

# URL til ressursen du ønsker å nå
url="http://mittapi.no/ressurs"

# Send forespørsel med grunnleggende autentisering
resultat=$(curl -u $brukernavn:$passord $url)

# Skriv ut svaret fra serveren
echo "$resultat"
```

#### Eksempelutdata:
```
{"status":"suksess","melding":"Du har nå tilgang til beskyttede data!"}
```

## Dypdykk:
Grunnleggende autentisering har vært en del av HTTP-protokollen siden RFC 1945 (HTTP/1.0) og videreført i RFC 2617. Det er enkelt, men ikke spesielt sikkert siden brukernavn og passord sendes i klartekst kodet med Base64. I mange tilfeller erstattes det nå av mer robuste autentiseringssystemer som OAuth.

Alternativer til grunnleggende autentisering inkluderer:
- OAuth/OAuth2
- API-nøkler
- SAML
- JWT (JSON Web Tokens)

Implementeringsdetaljer:
- Den grunnleggende autentiseringstrengen er `Brukernavn:Passord` kodet i Base64.
- `curl` tilbyr et flagg `-u` for autentiseringstrengen, som tar seg av kodingen automatisk.
- For økt sikkerhet, bør HTTPS alltid brukes for å kryptere forespørselen.

## Se Også:
- cURL offisiell dokumentasjon: https://curl.se/docs/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- Grunnleggende autentisering på MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme
