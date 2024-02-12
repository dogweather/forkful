---
title:                "Debug-Ausgaben drucken"
aliases:
- /de/lua/printing-debug-output/
date:                  2024-01-20T17:52:59.152151-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Ausgeben von Debug-Informationen hilft beim Verstehen, was im Code passiert. Programmierer nutzen es, um Fehler schnell zu finden und den Programmfluss nachzuvollziehen.

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
