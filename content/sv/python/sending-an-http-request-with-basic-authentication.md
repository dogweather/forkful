---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Att skicka HTTP-förfrågan med grundläggande autentisering i Python

## Vad & Varför? 

HTTP-förfrågan med grundläggande autentisering är en teknik för att skicka data över nätverk och kontrollera användarens identitet. Programmerare gör detta för att skydda känslig data och begränsa åtkomst till vissa resurser.

## Hur man gör:

Python erbjuder `requests`-biblioteket för att enkelt skicka HTTP-förfrågningar. Nedan är ett exempel på hur man skickar en `GET` förfrågan med grundläggande autentisering.

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('https://example.com', auth=HTTPBasicAuth('user', 'pass'))

print(response.status_code)
```

Resultatet skrivs ut som en HTTP statuskod, till exempel `200` för en framgångsrik begäran.

## Djupt dyk

Grundläggande autentisering är en enkel och direkt metod som har använts sedan HTTP/1.0. Icke desto mindre, den erbjuder inte starkt skydd eftersom användarnamn och lösenord skickas i klartext.

Alternativ inkluderar Digest Authentication och OAuth. Digest autentisering hashar lösenordsdetaljerna innan de skickas över nätverket. OAuth använder tokens snarare än autentiseringsuppgifter.

Vid implementering, kom ihåg att vara uppmärksam på HTTP statuskoden som returneras. En `401` statuskod indikerar att autentisering misslyckades.

## Se också

För mer information om HTTP autentisering, se följande resurser:

- Python requests bibliotek dokumentation: http://docs.python-requests.org/en/master/user/authentication/
- RFC 2617, Basic och Digest Access Authentication: https://tools.ietf.org/html/rfc2617
- OAuth officiella webbplats: https://oauth.net/