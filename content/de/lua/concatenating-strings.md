---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
In der Programmierung ist das Zusammenfügen von Zeichenketten, auch als "String-Konkatenation" bekannt, ein Prozess, bei dem wir mehrere Zeichenketten zu einer einzigen Zeichenkette kombinieren. Programmierer nutzen diese Methode, um dynamische Inhalte zu erstellen oder Daten in einer für Menschen lesbaren Format auszugeben.

## Wie zu:
Lua ermöglicht das einfache Verbinden von Zeichenketten mit dem `..` Operator. Hier sehen Sie ein typisches Beispiel und sein Ausgabewert:

```Lua
str1 = "Hallo"
str2 = "Welt"
ergebnis = str1 .. " " .. str2
print(ergebnis)
```

Ausgabe:

```Lua
Hallo Welt
```

## TiefTauchen
Die String-Konkatenation war in Lua bereits seit seiner ersten Veröffentlichung im Jahr 1993 vorhanden, was die Lesbarkeit und Bedienbarkeit von Lua-Code verbessert hat. Alternativen zur Konkatenation sind die Verwendung von Formatierungsstrings oder das Aufteilen und späteres Zusammenfügen von Zeichenketten, jedoch ist die einfache Konkatenation häufig die benutzerfreundlichste Option. Unter der Haube verwendet Lua Buffer, um Zeichenketten effizient zusammenzuführen.

## Siehe auch
Die offizielle Lua-Dokumentation () bietet umfassende Infos zu Zeichenketten und ihren Manipulationen. Stack Overflow () ist ein guter Ort, um spezifischere Fragen zu stellen und Lösungen zu finden.