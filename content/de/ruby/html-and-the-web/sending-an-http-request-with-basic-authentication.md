---
date: 2024-01-20 18:02:32.647298-07:00
description: "HTTP-Anfragen mit Basic-Authentifizierung senden Daten \xFCber Benutzername\
  \ und Passwort im Header, um Zugriff auf gesch\xFCtzte Ressourcen zu erhalten.\u2026"
lastmod: '2024-03-13T22:44:54.400202-06:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen mit Basic-Authentifizierung senden Daten \xFCber Benutzername\
  \ und Passwort im Header, um Zugriff auf gesch\xFCtzte Ressourcen zu erhalten."
title: HTTP-Anfragen mit Basisauthentifizierung senden
weight: 45
---

## Was & Warum?
HTTP-Anfragen mit Basic-Authentifizierung senden Daten über Benutzername und Passwort im Header, um Zugriff auf geschützte Ressourcen zu erhalten. Programmierer nutzen das für einfache Authentifizierungsprozesse in Web-APIs.

## How to:
```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/secrets')
username = 'foo'
password = 'bar'

request = Net::HTTP::Get.new(uri)
request.basic_auth(username, password)

response = Net::HTTP.start(uri.hostname, uri.port) { |http| http.request(request) }

puts response.body
```
Beispiel-Ausgabe:
```
Geheime Informationen
```

## Deep Dive
Basic Authentication ist ein Veteran unter den Authentifizierungsmethoden und Teil des HTTP/1.0-Standards (RFC 1945). Heutzutage gibt es sicherere Alternativen wie OAuth, die in modernen Anwendungen bevorzugt werden. Die Implementierung ist simpel: Der `Authorization`-Header wird mit `Basic ` und einem Base64-kodierten String von Benutzername und Passwort ergänzt. Trotz seiner Einfachheit solltest du Basic Auth über HTTPS verwenden, um die Credentials zu schützen.

## See Also
- [RFC 7617 – The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Anleitung zur sicheren Nutzung von Basic Auth](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Alternativen zu Basic Authentication (OAuth 2.0)](https://oauth.net/2/)
