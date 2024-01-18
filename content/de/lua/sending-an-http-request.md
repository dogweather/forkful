---
title:                "Das Versenden einer http-Anfrage"
html_title:           "Lua: Das Versenden einer http-Anfrage"
simple_title:         "Das Versenden einer http-Anfrage"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTTP-Anfragen sind eine Möglichkeit für Programmierer, Daten von externen Servern abzurufen. Dies kann verwendet werden, um Informationen von einer Web-API zu erhalten oder um auf Webinhalte zuzugreifen. Es ist ein wesentlicher Bestandteil von vielen Anwendungen, die auf das Internet zugreifen.

## Wie geht das?

Der folgende Code zeigt, wie man mit Lua eine HTTP-Anfrage sendet:

```lua
local http = require("socket.http") -- Lua-Modul zum Senden von HTTP-Anfragen

local response = http.request("https://www.example.com") -- Sendet eine Anfrage an die angegebene URL und speichert die Antwort

print(response) -- Gibt die erhaltenen Daten aus
```

Output:
```html
<!DOCTYPE html>
<html>
<head>
  <title>Beispiel Webseite</title>
</head>
<body>
  <h1>Willkommen bei unserem Beispiel!</h1>
</body>
</html>
```

## Tief tauchen

In den frühen Tagen des Internets war das Senden von HTTP-Anfragen sehr komplex und erforderte viel manuelle Arbeit. Mit der Einführung von Lua und anderen Programmiersprachen wurde dieser Prozess jedoch vereinfacht. Es gibt auch alternative Ansätze wie die Verwendung von Web-Frameworks, die das Senden von HTTP-Anfragen noch einfacher machen können. Die Implementation von HTTP-Anfragen in Lua basiert auf dem Standardmodul "socket.http" und nutzt die TCP/IP-Verbindung, um Daten mit dem Server auszutauschen.

## Siehe auch

- [Lua-Dokumentation zu HTTP-Anfragen](http://www.lua.org/manual/5.3/manual.html#6.4)
- [Einfache Möglichkeit, mit Lua auf API-Daten zuzugreifen](https://www.youtube.com/watch?v=x5MOgOqM0EY)
- [LuaSocket-Modul](https://github.com/diegonehab/luasocket) für erweiterte HTTP-Funktionen