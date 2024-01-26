---
title:                "HTML parsen"
date:                  2024-01-20T15:32:33.737464-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parser ermöglichen das Auslesen und Verarbeiten von HTML-Dateien. Programmierer nutzen sie, um Daten automatisiert zu sammeln oder Inhalte von Webseiten zu manipulieren.

## How to:
Um HTML in Lua zu parsen, nutzt man häufig Bibliotheken wie `lua-html` oder `luaxml`. Hier ein einfaches Beispiel mit `lua-html`:

```lua
local html = require("lua-html")

local text = [[
<html>
<head><title>Testseite</title></head>
<body><p>Hallo, Welt!</p></body>
</html>
]]

local parsed_html = html.parse(text)
local paragraphs = parsed_html:query_selector('p')

for _, p in ipairs(paragraphs) do
    print(p:get_text())
end
```

Ausgabe:
```
Hallo, Welt!
```

## Deep Dive
HTML-Parser gab es schon, seitdem Webseiten entwickelt werden. Sie sind kritisch für Suchmaschinen und Datenanalyse-Tools. Während Lua nicht die Hauptwahl für Web-Scraping ist, bietet es doch effiziente Libraries dafür. Alternativen zu Lua-Parsern sind BeautifulSoup (Python) oder Nokogiri (Ruby). Die Herausforderung beim Parsen von HTML ist das Handling von schlecht geformtem HTML, was oft zu Parser-spezifischer Logik führt.

## See Also
- Lua-html Github: https://github.com/tarantool/lua-html
- LuaXML GitHub: https://github.com/LuaDist/luaxml
- W3C HTML Parser Spezifikationen: https://html.spec.whatwg.org/multipage/parsing.html
