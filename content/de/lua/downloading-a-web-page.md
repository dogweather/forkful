---
title:                "Webseite herunterladen"
html_title:           "Lua: Webseite herunterladen"
simple_title:         "Webseite herunterladen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bezieht sich auf den Prozess des Abrufens und Speicherns von Dateien, die Teil einer bestimmten Webseite sind. Programmierer nutzen diesen Vorgang, um Inhalte aus dem Internet für ihre Anwendungen oder Programme zu extrahieren und zu verwenden.

## Wie geht das?
```
-- Hier ist ein Beispiel, wie man eine Webseite in Lua herunterladen kann:
local http = require("socket.http")
local body, code, headers = http.request("https://www.example.com")

print("HTTP Code: ", code)
print("Headers:")
for k, v in pairs(headers) do
    print(k, v)
end
print("Body: ", body)
```
**Ausgabe:**
```
HTTP Code: 200 OK
Headers:
Content-Type    text/html; charset=UTF-8
Content-Length  1254
Date            Tue, 06 Apr 2021 12:00:00 GMT
Server          Apache
Connection      close
Body: <html>
<head>
<title>Example Domain</title>
...
```

## Tiefergehende Informationen
Das Herunterladen von Webseiten ist seit den Anfängen des Internets ein wichtiger Bestandteil der Programmierung. Es gibt auch alternative Ansätze wie z.B. das Scraping von Inhalten oder API-Zugriffe. In Lua wird in der Regel die Bibliothek "socket.http" verwendet, um Webseiten herunterzuladen. Es ist auch möglich, externe Bibliotheken oder Frameworks zu verwenden, um den Prozess zu vereinfachen.

## Weitere Quellen
- Dokumentation zu socket.http: https://w3.impa.br/~diego/software/luasocket/http.html
- Ein einfaches Web-Scraping-Beispiel mit Lua: https://stackoverflow.com/questions/60413538/lua-webscraping-fails-on-httpsi-cannot-load-the-webpage
- Ein umfangreiches Framework für den Webzugriff in Lua: https://github.com/alua-dev/alua