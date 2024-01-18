---
title:                "Suchen und Ersetzen von Text"
html_title:           "Lua: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Schreiben von Code ist es oft notwendig, bestimmte Textpassagen zu suchen und durch andere zu ersetzen. Dies wird als "Suchen und Ersetzen" bezeichnet, und Programmierer tun dies, um ihren Code effizienter zu gestalten und Fehler zu vermeiden.

## Wie geht's?
Das Suchen und Ersetzen von Text in Lua kann mit Hilfe der Funktion "string.gsub" durchgeführt werden. Diese Funktion hat zwei Parameter: den zu suchenden Text und den zu ersetzenden Text. Zum Beispiel:

```Lua
local text = "Hallo Welt"
local new_text = string.gsub(text, "Welt", "Lua Nutzer")
print(new_text) -- Ausgabe: Hallo Lua Nutzer
```

## Tiefere Einblicke
Suchen und Ersetzen ist eine weit verbreitete Funktion in fast allen Programmiersprachen und existiert schon seit den Anfängen des Computers. In Lua gibt es auch alternative Funktionen wie "string.find" und "string.match", die die Suche nach Text auf bestimmte Muster und Patterns einschränken.

## Sieh dir auch an
- Die offizielle Lua-Dokumentation zu string.gsub: https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub
- Die Verwendung von string.gsub in einem realen Anwendungsbeispiel: https://www.script-tutorials.com/find-and-replace-text-with-lua/