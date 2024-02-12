---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
aliases:
- /de/python/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:27.566128-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung senden bedeutet, Benutzername und Passwort sicher mitzuschicken, um Zugang zu geschützten Ressourcen zu bekommen. Programmierer nutzen dies, um APIs oder Web-Dienste zu authentifizieren, die einfache Anmeldeinformationen erfordern.

## So geht's:
Hier ist ein Beispiel, wie man eine HTTP-Anfrage mit Basisauthentifizierung in Python verschickt:

```python
import requests
from requests.auth import HTTPBasicAuth

# Setze deine Anmeldeinformationen
benutzername = 'DeinBenutzer'
passwort = 'DeinPasswort'

# Die URL der geschützten Ressource
url = 'https://deinegeschützte.seite/ressource'

# Schicke die GET-Anfrage mit Basisauthentifizierung
antwort = requests.get(url, auth=HTTPBasicAuth(benutzername, passwort))

# Ich hab's! Schau dir den Inhalt an
print(antwort.status_code)
print(antwort.text)
```

Das könnte so aussehen:

```
200
{"nachricht": "Hallo, Du hast es geschafft!"}
```

## Tiefgang:
Ursprünglich entworfen für die Frühzeit des Internets, ist Basisauthentifizierung ein direkter, wenn auch nicht der sicherste Weg, Zugangskontrolle zu implementieren. Alternativen heute umfassen OAuth, API-Schlüssel oder JWTs (JSON Web Tokens), die in vielen Fällen mehr Sicherheit bieten. Bei der Nutzung von Basisauthentifizierung wird das Passwort in Base64 kodiert, jedoch nicht verschlüsselt, was es anfällig für Man-in-the-Middle-Angriffe macht. HTTPS sollte immer zusammen mit Basisauthentifizierung verwendet werden, um die Anmeldeinformationen zu schützen.

## Siehe auch:
- Requests Dokumentation: https://docs.python-requests.org/en/latest/
- HTTP-Authentifizierungsstandards: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Sicherheit von Authentifizierungsmethoden: https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html
