---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Eine HTTP-Anfrage mit Basic Authentication in Lua senden

## Was & Warum?
Die Abfrage einer HTTP-Anfrage mit Basic Authentication in Lua ist ein Prozess, bei dem Benutzername und Passwort in einem Standard HTTP-Header gesendet werden. Unsere Anwendungen brauchen dies oft, um Daten von sicheren Servern abzurufen.

## So geht's:
Sie können die Lua-HTTP-Bibliothek, `lua-http`, verwenden, um Anfragen mit Basic Authentication zu senden. Hier ist ein einfaches Beispiel, wie Sie dies tun können:

```Lua
-- Bibliothek importieren
local http_request = require "http.request"

-- HTTP-Anfrage erstellen
local req = http_request.new_from_uri("http://meinwebsite.com/resource")

-- Basic Authentication hinzufügen
req.headers:upsert(":authorization", "Basic " .. ("benutzername:passwort"):base64())

-- Die Anfrage senden
local headers, stream = req:go()
print(headers)
```

## Tiefere Erkenntnisse
Basic Authentication ist eines der ältesten und einfachsten Methoden zur Authentifizierung von HTTP-Anforderungen, aber es hat seine Nachteile, wie die Übertragung von Anmeldeinformationen in Klartext. Alternativen sind OAuth und Token-basierte Authentifizierung, die sicherer sind. Die Implementationdetails für Basic Authentication in Lua verwenden einen einfachen `base64`-Encoded-String der Form "username:password".

## Siehe auch
- [Lua HTTP-Bibliothek (lua-http)](http://daurnimator.github.io/lua-http/)
- [Dokumentation zur HTTP-Authentifizierung](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Einen Überblick über OAuth 2.0](https://oauth.net/2/)