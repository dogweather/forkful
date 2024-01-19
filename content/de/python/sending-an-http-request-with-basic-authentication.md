---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Von Python aus HTTP-Anfragen mit Basic Authentication senden

## Was & Warum?
Eine HTTP-Anfrage mit Basic Authentication ist eine einfache Methode, um Benutzerdaten an einen Server zu senden. Programmierer nutzen sie oft, um sich mit Web-APIs zu verbinden, die eine Authentifizierung benötigen.

## Wie man:
Python stellt das `requests` Modul zur Verfügung, das unkompliziert für HTTP-Anfragen mit Basic Authentication verwendet werden kann. Hier ist ein einfaches Beispiel:

```Python
import requests
from requests.auth import HTTPBasicAuth

response = requests.get('http://example.com', auth=HTTPBasicAuth('username', 'password'))

print(response.status_code)
print(response.content)
```
Dieses Skript sendet eine HTTP GET-Anfrage an 'http://example.com' mit einem Basic Authentication Header. Die Antwort wird anschließend gedruckt.

## Vertiefung
Ein Stück Geschichte: Basic Authentication ist seit den Anfängen des HTTP-Protokolls Teil dieses Standards, da sie eine einfache Art der Authentifizierung darstellt.

Alternativen: Abhängig vom Sicherheitsbedarf gibt es sicherere Authentifizierungsmethoden, wie OAuth oder JWT.

Implementierungsdetails: Python `requests` Bibliothek kodiert den Benutzernamen und das Passwort mit Base64 und fügt diesen String in den Header der HTTP-Anfrage ein.

## Siehe auch
Weitere Informationen über HTTP Basic Authentication und das Python `requests` Modul finden Sie unter den folgenden Links:
- [Python `requests` Documentation](https://requests.readthedocs.io/en/master/)
- [HTTP Basic Authentication on MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)