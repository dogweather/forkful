---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite bedeutet, deren Inhalt auf Ihrem Rechner zu speichern. So testen Programmierer z.B. ihre Tools in einer kontrollierten Umgebung oder sammeln Daten für die Bearbeitung.

## So geht's:

Mit Lua und der Bibliothek "luasocket" sowie dem Modul "http" kann man eine Webseite herunterladen:

```Lua
-- Pakete importieren
http = require("socket.http")
ltn12 = require("ltn12")

-- Datei zum Speichern öffnen
file = io.open("webInhalt.txt", "w")

-- Anfrage, Seite herunterladen
http.request{
  url = "http://example.com",
  sink = ltn12.sink.file(file)
}
```

Der Inhalt von "http://example.com" wird in "webInhalt.txt" gespeichert.

## Tiefere Einblicke

Lua und Luasocket sind 1993 bzw. 2003 veröffentlicht worden. "luasocket" gilt als Standardnetzwerkbibliothek von Lua. Es bieten sich auch Alternativen wie "lua-http" oder "luacurl" an, je nach Anforderung und Umgebung.

Das Herunterladen einer Webseite mit Lua besteht aus einer HTTP-Anfrage an den Server, welcher die Seite hostet. Diese Anfrage wird von der "http.request"-Funktion verarbeitet. Die Pufferung der Antwort, um sie in eine Datei zu schreiben, wird durch "ltn12.sink.file" geleistet.

## Siehe auch

Um Lua und das Web-Scraping besser zu verstehen, gibt es hilfreiche Quellen im Internet:

Lua Handbuch: (https://www.lua.org/manual/5.4/) 

Luasocket Dokumentation: (https://github.com/diegonehab/luasocket)

Tutorial zu ltn12: (http://lua-users.org/wiki/FiltersSourcesAndSinks)