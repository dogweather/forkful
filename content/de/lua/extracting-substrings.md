---
title:                "Substrings extrahieren"
html_title:           "Lua: Substrings extrahieren"
simple_title:         "Substrings extrahieren"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilzeichenfolgen ist eine häufig verwendete Technik in der Programmierung, bei der Teile einer Zeichenfolge entfernt und in eine neue Variable gespeichert werden. Programmierer verwenden dies oft, um bestimmte Informationen aus längeren Zeichenfolgen zu isolieren oder um Texte zu manipulieren.

## Wie funktioniert es?
Um eine Teilzeichenfolge in Lua zu extrahieren, kann die Funktion `string.sub()` verwendet werden. Dieser Funktion werden zwei Parameter übergeben: die Zeichenfolge, aus der die Teilzeichenfolge extrahiert werden soll, und die Position der ersten und letzten Zeichen, die extrahiert werden sollen. Es ist auch möglich, negative Indizes zu verwenden, um die Position der Zeichen von der rechten Seite der Zeichenfolge aus zu zählen.

```Lua 
-- Beispiel 1 
local str = "Dies ist ein Beispieltext" -- Zeichenfolge 
local sub = string.sub(str, 6, 14) -- " ist ein B" 

-- Beispiel 2 
local str = "Hallo Welt!" 
local sub = string.sub(str, 2, -3) -- "allo Wel" 
```

## Tiefere Einblicke 
Das Extrahieren von Teilzeichenfolgen ist schon lange eine wichtige Technik in der Programmierung und wird in verschiedenen Sprachen verwendet. In Lua können auch reguläre Ausdrücke verwendet werden, um Teilzeichenfolgen zu finden und zu extrahieren. Es gibt auch alternative Funktionen wie `string.match()`, die Teilzeichenfolgen basierend auf einem regulären Ausdruck suchen und möglicherweise mehrere Fundstellen zurückgeben können.

## Siehe auch 
- Offizielle Lua-Dokumentation zu string.sub(): https://www.lua.org/manual/5.3/manual.html#pdf-string.sub 
- Einführung in reguläre Ausdrücke mit Lua: https://www.lua.org/pil/20.2.html