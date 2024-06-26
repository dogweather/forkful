---
date: 2024-01-20 18:00:23.507324-07:00
description: "How to: (Wie man's macht:) HTTP-Anfragen sind seit den fr\xFChen 90er\
  \ Jahren Teil des Internets \u2013 entscheidend f\xFCr die Kommunikation zwischen\
  \ Client und\u2026"
lastmod: '2024-04-05T22:51:08.040235-06:00'
model: gpt-4-1106-preview
summary: "(Wie man's macht:) HTTP-Anfragen sind seit den fr\xFChen 90er Jahren Teil\
  \ des Internets \u2013 entscheidend f\xFCr die Kommunikation zwischen Client und\
  \ Server."
title: Einen HTTP-Request senden
weight: 44
---

## How to: (Wie man's macht:)
```Python
# Python 3.10 oder neuer benötigt
import requests

# GET-Anfrage um Daten zu erhalten
response = requests.get('https://api.github.com')

# Informationen auf der Konsole ausgeben
print(response.status_code)  # Status-Code
print(response.headers['content-type'])  # Antwort-Typ
print(response.json())  # JSON-Inhalt, wenn verfügbar

# POST-Anfrage um Daten zu senden
data = {'key': 'value'}
post_response = requests.post('https://httpbin.org/post', data=data)
print(post_response.text)  # Antwort-Text
```

## Deep Dive (Tiefere Einblicke)
HTTP-Anfragen sind seit den frühen 90er Jahren Teil des Internets – entscheidend für die Kommunikation zwischen Client und Server. Ursprünglich simpel für HTML-Dokumente gedacht, sind sie heute komplex für APIs und Web-Applikationen.

Alternativen zu `requests` sind `http.client` (in Python eingebaut) oder externe Bibliotheken wie `httpx`. `requests` gilt als benutzerfreundlich, `httpx` bietet async Unterstützung.

Die Implementierung einer Anfrage beinhaltet oft:
- Aufbau einer Session für wiederholte Anfragen
- Verwendung von Parametern und Headers für spezifische Anfrage-Details
- Handhabung von Response-Codes und Ausnahmebehandlungen

## See Also (Siehe auch)
- `requests` Dokumentation: https://docs.python-requests.org/en/master/
- Python `http.client` Dokumentation: https://docs.python.org/3/library/http.client.html
- `httpx`, eine async-fähige Alternative: https://www.python-httpx.org/
