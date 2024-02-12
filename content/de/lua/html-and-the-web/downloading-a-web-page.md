---
title:                "Webseite herunterladen"
aliases: - /de/lua/downloading-a-web-page.md
date:                  2024-01-20T17:44:30.451147-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Webseiten herunterladen bedeutet, den Inhalt einer Webseite über das Internet auf deinen Computer zu übertragen. Programmierer tun dies, um Daten zu extrahieren, automatisch zu verarbeiten oder Software-Tests durchzuführen.

## Vorgehensweise:
Um eine Webseite in Lua herunterzuladen, verwendest du die `socket.http`-Bibliothek. Im Beispiel unten siehst du, wie man den Inhalt einer Webseite anfragt und erhält:

```Lua
local http = require("socket.http")
local url = "http://www.beispielwebseite.de"

local body, statusCode, headers, statusText = http.request(url)

if statusCode == 200 then
    print("Webseite erfolgreich heruntergeladen!")
    print(body)  -- Gibt den Inhalt der Webseite aus.
else
    print("Fehler beim Herunterladen der Webseite: " .. statusText)
end
```
Wenn alles klappt, gibt `body` den HTML-Code der Seite zurück.

## Tiefgang:
Zurück in den 2000ern waren HTTP-Anfragen in Lua eher mühsam und benötigten externe Werkzeuge wie `curl` oder `wget`. Mit dem Aufkommen von `LuaSocket` wurde das Herunterladen von Webseiten direkt in Lua umsetzbar. Es gibt auch Alternativen wie `LuaSec` für HTTPS-Verbindungen. Wichtig ist die Behandlung von Header-Informationen und Statuscodes, um auf Ereignisse wie Umleitungen oder Serverfehler reagieren zu können.

## Siehe Auch:
- LuaSocket Dokumentation: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec (für HTTPS) GitHub-Seite: https://github.com/brunoos/luasec/wiki
- HTTP-Statuscodes: https://de.wikipedia.org/wiki/HTTP-Statuscode
