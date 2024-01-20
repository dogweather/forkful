---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP Anfragen in Lua: Ein Schneller Überblick

## Was & Warum?

Das Senden einer HTTP-Anfrage ist eine Aktion, bei der ein Client Daten von einem Server anfordert. Programmierer tun dies, um Daten aus dem Web zu extrahieren oder um serverseitige Aktionen auszulösen.

## So geht's:

In Lua verwenden wir meistens das `socket.http` Modul für HTTP-Anfragen. Hier ist ein einfaches Beispiel:

```Lua
http = require("socket.http")
url = "https://example.com"
response, status_code, header = http.request(url)
print(response)
```

Lauf dieses Skript und du wirst die Antwort des Servers auf deinem Bildschirm sehen.

## Deep Dive

HTTP-Anfragen sind seit dem Beginn des Internets im Einsatz. Um HTTP-Anfragen in Lua zu ermöglichen, wurde das `socket.http` Modul eingeführt, das Teil der LuaSocket Bibliothek ist.

Obwohl `socket.http` das am häufigsten genutzte Modul zum Senden von HTTP-Anfragen in Lua ist, gibt es auch Alternativen wie `luajit-request` oder `lua-http`.

Eines der interessantesten Merkmale von `socket.http` ist die Möglichkeit, benutzerdefinierte Header und weitere Optionen in den Anfragen zu setzen. Hier findest du einen Beispiel dafür:

```Lua
http = require("socket.http")
http.TIMEOUT = 2
response, status_code, header = http.request
{
  url = "https://example.com",
  method = "GET",
  headers = 
  {
    ["Content-Type"] = "application/json",
    ["Cookie"] = "key=value"
  }
}
print(response)
```

In diesem Code fügen wir einen "Content-Type" und "Cookie" Header zu unserer Anfrage hinzu und setzen einen Timeout von 2 Sekunden für die Anfrage.

## Siehe Auch

Für weitere Informationen, siehe diese Links:

- [LuaSocket HTTP Documentation](https://w3.impa.br/~diego/software/luasocket/http.html)
- [LuaJIT Request GitHub Page](https://github.com/LPGhatguy/luajit-request)
- [Lua HTTP GitHub Page](https://github.com/daurnimator/lua-http)