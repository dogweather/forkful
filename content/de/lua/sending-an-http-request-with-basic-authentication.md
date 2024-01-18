---
title:                "Eine http-Anfrage mit Basisauthentifizierung senden"
html_title:           "Lua: Eine http-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine http-Anfrage mit Basisauthentifizierung senden"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage mit einfacher Authentifizierung ist eine Möglichkeit für Programmierer, sich bei einer Website oder API zu authentifizieren. Dies bedeutet, dass sie sich als autorisiertes Benutzerkonto ausweisen, um Zugriff auf geschützte Inhalte oder Funktionen zu erhalten.

## So geht's:
```
local username = "Benutzername"
local password = "Passwort"
local url = "https://www.example.com/api"
local request = http.request {
    url = url,
    method = "POST",
    headers = {
        ["Authorization"] = "Basic " .. mime.b64(username .. ":" .. password)
    },
    data = "payload=Hello"
}

local response = request()
print(response.statuscode)
print(response.content)
```

Erklärung: Zunächst werden die Login-Daten des Benutzers sowie die zielgerichtete URL festgelegt. Dann wird eine HTTP-Anfrage mit der Methode "POST" erstellt und die erforderlichen Authentifizierungs-Header hinzugefügt. Der Parameter "data" ermöglicht das Hinzufügen von Inhalt zur Anfrage. Schließlich wird die Anfrage ausgeführt und die Statuscode- und Inhaltsinformationen des Ergebnisses werden ausgegeben.

## Tiefentauchen:
Bevor das Konzept der einfachen Authentifizierung eingeführt wurde, wurde die Authentifizierung über HTTP-Digest verwendet. Im Vergleich dazu ist die einfache Authentifizierung weniger sicher, da die Login-Daten im Klartext übertragen werden.
Es gibt auch alternative Methoden wie die OAuth-Authentifizierung, die speziell für APIs entwickelt wurde und eine bessere Sicherheit bietet.
Bei der Implementierung der einfachen Authentifizierung müssen die Login-Daten in einen Base64-String kodiert und mit dem "Basic" Prefix im HTTP-Header gesendet werden.

## Siehe auch:
- Lua Anleitung zur HTTP-Bibliothek: https://www.lua.org/pil/22.3.html
- Informationen zu Basic Authentifizierung: https://developer.mozilla.org/de/docs/Web/HTTP/Authentication
- Weitere Details zur OAuth-Authentifizierung: https://oauth.net/about/introduction/