---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:24.323299-07:00
description: "Das Parsen von HTML beinhaltet das Extrahieren von Daten und Informationen\
  \ aus HTML-Dokumenten, was f\xFCr das Web-Scraping, die Datenanalyse und\u2026"
lastmod: 2024-02-19 22:05:12.943368
model: gpt-4-0125-preview
summary: "Das Parsen von HTML beinhaltet das Extrahieren von Daten und Informationen\
  \ aus HTML-Dokumenten, was f\xFCr das Web-Scraping, die Datenanalyse und\u2026"
title: HTML parsen
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML beinhaltet das Extrahieren von Daten und Informationen aus HTML-Dokumenten, was für das Web-Scraping, die Datenanalyse und Automatisierungsaufgaben von entscheidender Bedeutung ist. Programmierer führen dies durch, um Webinhalte programmatisch zu sammeln, zu analysieren oder zu manipulieren, wodurch die Automatisierung dessen ermöglicht wird, was sonst eine manuelle Extraktion von Daten von Websites wäre.

## Wie geht das:
Lua verfügt nicht über eine integrierte Bibliothek zum Parsen von HTML, aber Sie können Drittanbieter-Bibliotheken wie `LuaHTML` nutzen oder Bindungen für `libxml2` durch `LuaXML` verwenden. Ein beliebter Ansatz ist die Verwendung der `lua-gumbo`-Bibliothek zum Parsen von HTML, die eine unkomplizierte, HTML5-konforme Parsing-Fähigkeit bietet.

### lua-gumbo installieren:
Stellen Sie zunächst sicher, dass `lua-gumbo` installiert ist. Sie können es in der Regel mit luarocks installieren:

```sh
luarocks install lua-gumbo
```

### Grundlegendes Parsen mit lua-gumbo:
So können Sie einen einfachen HTML-Schnipsel parsen und Daten daraus extrahieren mit `lua-gumbo`:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Hallo, Welt!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- Ausgabe: Hallo, Welt!
```

### Fortgeschrittenes Beispiel - Links extrahieren:
Um `href`-Attribute von allen Anker-Tags (`<a>`-Elementen) in einem HTML-Dokument zu extrahieren:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Beispielseite</title></head>
<body>
  <a href="http://beispiel.com/1">Link 1</a>
  <a href="http://beispiel.com/2">Link 2</a>
  <a href="http://beispiel.com/3">Link 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- Stellen Sie sicher, dass es ein Element ist und Attribute hat
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- Beispiel Ausgabe:
-- http://beispiel.com/1
-- http://beispiel.com/2
-- http://beispiel.com/3
```

Dieser Codeausschnitt iteriert durch alle Links im Dokument und druckt deren `href`-Attribute aus. Die Fähigkeit der `lua-gumbo`-Bibliothek, die Struktur eines HTML-Dokuments zu parsen und zu verstehen, vereinfacht den Prozess der Extraktion spezifischer Elemente basierend auf ihren Tags oder Attributen.
