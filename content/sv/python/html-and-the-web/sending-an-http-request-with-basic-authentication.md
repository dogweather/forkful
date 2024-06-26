---
date: 2024-01-20 18:02:36.364267-07:00
description: "Hur man g\xF6r: S\xE4tta upp och g\xF6ra en HTTP-beg\xE4ran i Python\
  \ med basic auth \xE4r enkelt. Anv\xE4nd `requests`-biblioteket f\xF6r effektivitet."
lastmod: '2024-03-13T22:44:37.482720-06:00'
model: gpt-4-1106-preview
summary: "S\xE4tta upp och g\xF6ra en HTTP-beg\xE4ran i Python med basic auth \xE4\
  r enkelt."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

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
