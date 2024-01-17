---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
html_title:           "Python: Å sende en HTTP-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Hva & hvorfor?

Når du sender en HTTP-forespørsel med grunnleggende autentisering, legger du til en ekstra del i forespørselsheaderen som spesifiserer brukernavn og passord. Dette er en vanlig måte for programmerere å autentisere seg mot et nettsted eller en server.

Hvorfor gjør vi dette? Fordi det gir sikker kommunikasjon mellom klient og server. Med grunnleggende autentisering blir informasjonen kryptert før den sendes over nettverket, noe som gjør den mindre sårbar for uautorisert tilgang.

Hvordan:

```Python
import requests
from requests.auth import HTTPBasicAuth

url = 'http://www.example.com'
username = 'brukernavn'
password = 'passord123'

response = requests.get(url, auth=HTTPBasicAuth(username, password))

print(response.text)
```

Deep Dive:

HTTP-forespørsler med grunnleggende autentisering har vært en standard autentiseringsmetode siden tidlig på 90-tallet. Den bruker en Base64-koding for å konvertere brukernavn og passord til en streng som legges til i forespørselsheaderen.

En alternativ metode er å bruke OAuth-autentisering, som lar brukerne gi tilgang til sine ressurser uten å gi ut passordet sitt. Implementering av grunnleggende autentisering i Python er enkelt ved hjelp av Requests-biblioteket og HTTPBasicAuth-modulen.

Se også:

- Dokumentasjon for Requests biblioteket: https://requests.readthedocs.io/en/master/
- Informasjon om HTTP grunnleggende autentisering: https://www.ietf.org/rfc/rfc2617.txt