---
date: 2024-01-20 18:00:24.222210-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel betyr \xE5 sp\xF8rre en server om\
  \ info via internett. Programmerere gj\xF8r det for \xE5 hente data, som nettsideinnhold\
  \ eller API-svar."
lastmod: 2024-02-19 22:04:59.638942
model: gpt-4-1106-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel betyr \xE5 sp\xF8rre en server om info\
  \ via internett. Programmerere gj\xF8r det for \xE5 hente data, som nettsideinnhold\
  \ eller API-svar."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel betyr å spørre en server om info via internett. Programmerere gjør det for å hente data, som nettsideinnhold eller API-svar.

## Hvordan:

Python har flere biblioteker for HTTP-forespørsler. La oss bruke `requests`, et populært alternativ. Installer det først med `pip install requests`.

```Python
import requests

# Gjør en GET-forespørsel
response = requests.get('https://api.github.com')

# Sjekk statuskode
print(response.status_code)  # 200 betyr suksess

# Utskrift av svaret
print(response.json())  # JSON-innhold
```

Resultat:

```
200
{'current_user_url': 'https://api.github.com/user', ...}
```

## Dypdykk:

HTTP-forespørsler dateres tilbake til internettets oppstart, med HTTP/1.1 i 1997 som standarden. I dag brukes også HTTP/2 og er under utvikling. Alternativer til `requests` inkluderer biblioteker som `http.client` og `urllib.request` i standardbiblioteket. I `requests`, håndterer `Session`-objekter gjenbruk av forbindelser for å øke effektiviteten, mens `Response`-objektet gir tilgang til data som statuskoder og svarinnhold.

## Se Også:

- Requests dokumentasjon: https://docs.python-requests.org/
- HTTP statuskoder: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
- Python `http.client` dokumentasjon: https://docs.python.org/3/library/http.client.html
- Python `urllib.request` tutorial: https://docs.python.org/3/howto/urllib2.html
