---
date: 2024-01-20 18:02:29.512857-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ at vi tilf\xF8yer brukernavn og passord for \xE5 f\xE5 tilgang til en ressurs.\
  \ Vi gj\xF8r dette for\u2026"
lastmod: '2024-02-25T18:49:38.588743-07:00'
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering betyr\
  \ at vi tilf\xF8yer brukernavn og passord for \xE5 f\xE5 tilgang til en ressurs.\
  \ Vi gj\xF8r dette for\u2026"
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering betyr at vi tilføyer brukernavn og passord for å få tilgang til en ressurs. Vi gjør dette for sikkerhetskontroll, slik at bare autoriserte brukere får tilgang.

## Hvordan å:
```Python
import requests
from requests.auth import HTTPBasicAuth

# Erstatt 'brukernavn' og 'passord' med dine egne legitimasjoner.
response = requests.get('https://ditt-eksempel-url.com', auth=HTTPBasicAuth('brukernavn', 'passord'))

# Utskrift av responsen
print(response.status_code)
print(response.content)
```

Hvis alt går bra, bør du se noe slik:
```
200
b'Innholdet i den beskyttede ressursen'
```

## Dypdykk
Grunnleggende autentisering er en del av HTTP-protokollen, anvendt siden det tidlige internettet, og er bra for enkle autentiseringsscenarioer. Moderne alternativer som OAuth gir bedre sikkerhet. Basic auth sender base64-kodet brukernavn og passord i hver forespørsel, noe som gjør HTTPS nødvendig for å unngå avlytting.

## Se Også
- Requests dokumentasjon om autentisering: https://requests.readthedocs.io/en/master/user/authentication/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- MDN Web Docs om grunnleggende autentisering: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
