---
date: 2024-01-20 17:52:59.152151-07:00
description: 'How to: Um in Lua Debug-Informationen auszugeben, verwendet man die
  `print()` Funktion. Hier ein paar Beispiele.'
lastmod: '2024-03-13T22:44:54.017788-06:00'
model: gpt-4-1106-preview
summary: Um in Lua Debug-Informationen auszugeben, verwendet man die `print()` Funktion.
title: Debug-Ausgaben drucken
weight: 33
---

## How to:
Um in Lua Debug-Informationen auszugeben, verwendet man die `print()` Funktion. Hier ein paar Beispiele:

```lua
print("Hallo Welt")  -- Einfache Nachricht
print(123)           -- Zahlenausgabe
local vari = "Test"
print(vari)          -- Variable ausgeben
```

Die Ausgabe sieht dann so aus:

```
Hallo Welt
123
Test
```

## Deep Dive:
Seit Lua 5.1 gibt es die `print()` Funktion, die an die Standardausgabe sendet. Alternativen wie `io.write()` erlauben feinere Kontrolle, z.B. keinen Zeilenumbruch anzuhängen. Für umfangreichere Debugging-Aufgaben können spezielle Bibliotheken verwendet werden, die mehr Funktionen als die eingebaute `print()` anbieten.

## See Also:
- [Online Lua-Dokumentation](https://www.lua.org/manual/5.4/)
- [Programming in Lua (Erste Ausgabe)](http://www.lua.org/pil/contents.html)
