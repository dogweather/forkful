---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parsing ist der Prozess, bei dem der Quelltext einer Webseite gelesen und analysiert wird, um bestimmte Informationen herauszufiltern und nutzbar zu machen. Programmierer machen dies, um Webinhalte zu sammeln, zu modifizieren oder zu manipulieren.

## Wie geht das:

Lua hat eine Bibliothek namens LuaHTML, die HTML-Parsing ermöglicht. Hier ist ein einfaches Beispiel, um den Inhalt eines HTML-Tags zu lesen.

```Lua
luaHTML = require("luahtml") 
 -- Auf den HTML-Inhalt zugreifen
HTML_content = '<h1>Willkommen zu Lua HTML Parsing </h1>'
parsed_HTML = luaHTML.parse(HTML_content)
print(parsed_HTML:h1())  -- Output: Willkommen zu Lua HTML Parsing
```
In diesem kleinen Abschnitt, nutzten wir die Funktion 'parse', um auf den HTML-Inhalt zuzugreifen und den Inhalt des `h1`-Tags zu drucken.

## Vertiefung:

Die Geschichte des HTML-Parsing ist spannend. HTML wurde ursprünglich von Tim Berners-Lee in den 1990er Jahren entwickelt. Seitdem ist die Notwendigkeit entstanden, HTML zu analysieren, um auf den riesigen Umfang an im Web verfügbaren Daten zuzugreifen.

Es gibt verschiedene alternativen zu Lua zum Parsen von HTML wie Python mit BeautifulSoup oder Jsoup in Java.

Die Implementierung von HTML-Parsing in Lua ist ziemlich einfach. Sie nutzen Library-Funktionen zum Abrufen von DOM-Elementen, die in der HTML-Struktur verwenden werden. 

## Siehe auch:

Für mehr Informationen besuchen Sie bitte:

LuaHTML: http://lua-users.org/wiki/LuaHtml

Lua Programmiersprache: https://www.lua.org

HTML Parsing mit Python: https://www.crummy.com/software/BeautifulSoup/bs4/doc/ 

HTML Parsing mit Java: https://jsoup.org