---
title:                "Die Länge eines Strings bestimmen"
html_title:           "Lua: Die Länge eines Strings bestimmen"
simple_title:         "Die Länge eines Strings bestimmen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Finden der Länge einer Zeichenkette ist eine häufige Aufgabe in der Programmierung. Es bezieht sich einfach darauf, die Anzahl der Zeichen in einer Zeichenkette zu bestimmen. Programmierer verwenden dies oft, um die Länge von Benutzereingaben zu überprüfen oder um Fehler im Code zu finden.

## Wie geht's?
Das Finden der Länge einer Zeichenkette ist in Lua sehr einfach durchzuführen. Wir verwenden einfach die Funktion ```string.len()```, die als Argument die Zeichenkette nimmt, deren Länge wir finden wollen. Hier ist ein Beispielcode und die entsprechende Ausgabe:

```Lua
local str = "Hallo Lua-Freunde"
print(string.len(str))
```
Output: 17

## Tiefer tauchen
Das Finden der Länge einer Zeichenkette ist eine grundlegende Funktion in jeder Programmiersprache und wurde schon seit den Anfängen der Programmierung verwendet. Es gibt auch alternative Ansätze, wie zum Beispiel die Verwendung von Schleifen, um jedes Zeichen in der Zeichenkette zu zählen. Diese Methode kann jedoch zeitaufwändiger sein und ist in der Regel nicht so effizient wie die Verwendung von ```string.len()```. In Lua unterstützt die Funktion auch Unicode-Zeichen und berücksichtigt die richtige Anzahl der Bytes, während einige andere Programmiersprachen nur die Anzahl der Zeichen bestimmen können.

## Siehe auch
- [Lua-Manual - string.len Funktion](https://www.lua.org/manual/5.4/manual.html#pdf-string.len)
- [Lua-Dokumentation zum Thema Zeichenketten](https://www.lua.org/pil/20.html)