---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Python: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Vad och varför?

Att skicka en HTTP-förfrågan med grundläggande autentisering är en metod som används av programmerare för att tillåta användare att logga in på en webbplats eller tillämpning genom att ange en användaruppgifter (användarnamn och lösenord). Detta är en standardmetod och används ofta som den mest enkla och pålitliga autentiseringsmetoden.

# Hur gör man?

```python
import requests

url = 'https://exempelwebbplats.com'
user = 'användarnamn'
password = 'lösenord'

response = requests.get(url, auth=(user, password))

print(response.text)
```

Resultat:

```
Volvo XC40 har vunnit årets bil 2018!
```

# Djupdykning

Denna autentiseringsmetod utvecklades först i HTTP-protokollet 1996 och har varit en standardmetod sedan dess. Andra autentiseringsmetoder inkluderar OAuth och digest autentisering, men grundläggande autentisering är fortfarande den mest använda metoden på grund av dess enkelhet och tillförlitlighet.

När du skapar en begäran skickar du dina användaruppgifter över nätverket i klartext, vilket kan vara en potentiell säkerhetsrisk. Det är därför grundläggande autentisering inte är rekommenderad för att skydda känslig information.

# Se även

- [Requests bibliotek för Python](https://requests.readthedocs.io/)
- [HTTP Basic Authentication - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)