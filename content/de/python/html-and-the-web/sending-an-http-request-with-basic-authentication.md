---
date: 2024-01-20 18:02:27.566128-07:00
description: 'So geht''s: Hier ist ein Beispiel, wie man eine HTTP-Anfrage mit Basisauthentifizierung
  in Python verschickt.'
lastmod: '2024-03-13T22:44:53.377935-06:00'
model: gpt-4-1106-preview
summary: Hier ist ein Beispiel, wie man eine HTTP-Anfrage mit Basisauthentifizierung
  in Python verschickt.
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

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
