---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Å Send HTTP forespørsel med grunnleggende autentisering i Python

## Hva & Hvorfor?
Å sende en HTTP forespørsel med grunnleggende autentisering innebærer å sende en forespørsel til en server sammen med brukernavn og passord. Dette brukes ofte av programmerere for å få tilgang til webressurser som krever bekreftelse av en brukers identitet.

## Hvordan?
Her er hvordan du gjør det i Python ved hjelp av `requests` biblioteket. Hvis du ikke har det installert, kjør `pip install requests`.

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('http://yoururl.com', auth=HTTPBasicAuth('brukernavn', 'passord'))

print(response.status_code)
```

Dette vil gi ut statuskoden for din HTTP forespørsel. For eksempel, `200` viser at forespørselen var vellykket.

## Dyp Dykk
Den grunnleggende autentiseringen protokollen oppstod tidlig i historien til webutvikling, da sikkerhet og skalering var mindre av en bekymring enn de er nå. Det er ganske enkelt, men det gir ikke en høy grad av sikkerhet, siden brukernavn og passord er kodet med base64 og kan enkelt dekodes.

Noen alternativer til grunnleggende autentisering inkluderer OAuth, som er mer komplisert å implementere men tilbyr bedre sikkerhet, og token-basert autentisering.

Når det gjelder å sende en HTTP forespørsel med grunnleggende autentisering i Python, bruker "requests" biblioteket 'HTTPBasicAuth' klassen til å håndtere autentiseringsdetaljene.

## Se Også
- Python "requests" Dokumentasjon: [https://requests.readthedocs.io](https://requests.readthedocs.io)
- "HTTP Authentication" på MDN web docs: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- OAuth Homepage: [https://oauth.net](https://oauth.net)