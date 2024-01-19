---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debugausgabe in Lua drucken

## Was & Warum?

Im Allgemeinen können wir Debug-Ausgaben mit der Funktion `print()` in Lua ausgeben. Wir machen das, um Problemstellen im Code leicht zu identifizieren und um schnelle und einfache Überprüfungen der Variablen während der Laufzeit zu ermöglichen.

## So geht's:

Wir können die `print()` Funktion nutzen, um Informationen auf der Konsole auszugeben. Sieh dir dieses einfache Beispiel an:

```Lua
-- Variablen deklarieren
local name = "John Doe"
local age = 32
-- Ausgabe der Variablen
print("Name: ", name)
print("Alter: ", age)
```

Wenn du dieses Skript ausführst, siehst du die folgende Ausgabe auf dem Bildschirm:

```Lua
Name:  John Doe
Alter: 32
```

## Vertiefung

Frühe Versionen von Lua hatten noch keine eingebauten Funktionen für die Debugausgabe. Programmierer mussten eigene Funktionen dafür schreiben. Heutzutage verwenden viele Lua-Programmierer die Funktion `print()`, da sie einfach zu verwenden und sehr effektiv ist. 
Es gibt allerdings auch alternativen wie die `io` Bibliothek, die mehr Kontrolle über die Ausgabe gibt. Und schließlich gibt es auch noch das `debug` Modul, dass einen detaillierten Einblick in die Interna von Lua ermöglicht, indem es auch verborgene Details der Variablen ausgibt.

## Siehe auch

- Der offizielle Lua-Leitfaden: https://www.lua.org/manual/5.4/manual.html
- Eine tiefgehende Erläuterung des `debug` Moduls: http://lua-users.org/wiki/DebugLibraryTutorial
- Eine nützliche Reihe von Lua-Tipps zur Fehlerbehebung: https://www.tutorialspoint.com/lua/lua_debugging.htm