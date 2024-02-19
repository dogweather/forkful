---
aliases:
- /sv/python/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:36.364267-07:00
description: "HTTP-beg\xE4ran med grundl\xE4ggande autentisering anv\xE4nds f\xF6\
  r att s\xE4kert skicka anv\xE4ndarnamn och l\xF6senord \xF6ver n\xE4tet. Programmerare\
  \ g\xF6r detta f\xF6r att\u2026"
lastmod: 2024-02-18 23:08:51.421552
model: gpt-4-1106-preview
summary: "HTTP-beg\xE4ran med grundl\xE4ggande autentisering anv\xE4nds f\xF6r att\
  \ s\xE4kert skicka anv\xE4ndarnamn och l\xF6senord \xF6ver n\xE4tet. Programmerare\
  \ g\xF6r detta f\xF6r att\u2026"
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
---

{{< edit_this_page >}}

## Vad & Varför?
HTTP-begäran med grundläggande autentisering används för att säkert skicka användarnamn och lösenord över nätet. Programmerare gör detta för att kontrollera användarens behörighet innan de tillåter åtkomst till skyddade resurser.

## Hur man gör:
Sätta upp och göra en HTTP-begäran i Python med basic auth är enkelt. Använd `requests`-biblioteket för effektivitet.

```python
import requests
from requests.auth import HTTPBasicAuth

url = 'https://ditt-api.se/v1/hemligt'
anvandarnamn = 'användare123'
losenord = 'superhemligt'

svar = requests.get(url, auth=HTTPBasicAuth(anvandarnamn, losenord))

print(svar.status_code)
print(svar.json())
```

Sample output könnte se ut så här:

```
200
{'hemlighet': 'Bakom denna dörr finner du skatter av information.'}
```

Säkerhetsnotis: Använd HTTPS för att skydda autentiseringsuppgifter i transit.

## Djupdykning
HTTP Basic Authentication, introducerat med HTTP/1.0, skickar credentials kodade i Base64. Det är enkelt, men inte det säkraste ens över HTTPS. Moderna alternativ inkluderar OAuth och JWT (JSON Web Tokens) som erbjuder robustare säkerhet. När du använder `requests`, hanterar biblioteket Base64-kodningen åt dig.

För breddad säkerhet, överväg tvåfaktorsautentisering eller API-nycklar. Bär tokens i 'Authorization' headern istället för användarnamn och lösenord för bättre skydd.

## Se även
- Requests dokumentation: https://requests.readthedocs.io/en/latest/
- OAuth 2.0: https://oauth.net/2/
- JWT: https://jwt.io/
- Python `http.client` för de som föredrar standardbibliotek: https://docs.python.org/3/library/http.client.html

Kom ihåg att följa bästa praxis och håll dig uppdaterad med de senaste säkerhetsrekommendationerna.
