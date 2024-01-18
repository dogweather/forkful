---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Lua: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Konvertieren einer Zeichenfolge in Kleinbuchstaben ist der Prozess, bei dem alle Großbuchstaben in einer Zeichenfolge in entsprechende Kleinbuchstaben umgewandelt werden. Programmierer machen dies aus verschiedenen Gründen, wie z.B. zur Konsistenz oder zur besseren Vergleichbarkeit von Zeichenfolgen.

## Wie geht das?
Es gibt zwei einfache Möglichkeiten, eine Zeichenfolge in Kleinbuchstaben umzuwandeln: die integrierte Funktion "string.lower()" und die Verwendung einer Schleife, um jeden Buchstaben individuell zu konvertieren. Hier sind Beispiele für beide Methoden:

```Lua
-- Mit "string.lower()"
local str = "TESTSTRING"
print(string.lower(str)) -- Ausgabe: teststring

-- Mit einer Schleife
local str = "TESTSTRING"
local result = ""

for i = 1, #str do
  result = result .. string.lower(string.sub(str, i, i))
end

print(result) -- Ausgabe: teststring
```

## Tiefere Einblicke
Die Funktion "string.lower()" ist Teil der Standardbibliothek von Lua und bietet eine einfache Möglichkeit, Zeichenfolgen in Kleinbuchstaben umzuwandeln. Alternativ können Programmierer auch die Funktion "string.upper()" verwenden, um Zeichenfolgen in Großbuchstaben umzuwandeln. Bei der Verwendung einer Schleife, um jeden Buchstaben individuell zu konvertieren, sollten Programmierer bedenken, dass es Unterschiede bei der Behandlung von Umlauten und Sonderzeichen geben kann, je nachdem, in welchem Kontext die Zeichenfolge verwendet wird.

## Siehe auch
Weitere Informationen zu Zeichenfolgenmanipulation und der integrierten Lua-String-Bibliothek finden Sie in der offiziellen Lua-Dokumentation unter https://www.lua.org/manual/5.4/manual.html#6.4. Weitere nützliche Funktionen für die Verarbeitung von Zeichenfolgen sind "string.gsub()", "string.reverse()", und "string.find()".