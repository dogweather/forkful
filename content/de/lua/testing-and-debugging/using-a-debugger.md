---
date: 2024-01-26 03:50:09.085077-07:00
description: "Wie: Lua kommt nicht mit einem eingebauten Debugger, aber Sie k\xF6\
  nnen externe Debugger verwenden, wie zum Beispiel ZeroBrane Studio. Hier ist ein\u2026"
lastmod: '2024-03-13T22:44:54.019667-06:00'
model: gpt-4-0125-preview
summary: "Lua kommt nicht mit einem eingebauten Debugger, aber Sie k\xF6nnen externe\
  \ Debugger verwenden, wie zum Beispiel ZeroBrane Studio."
title: Einsatz eines Debuggers
weight: 35
---

## Wie:
Lua kommt nicht mit einem eingebauten Debugger, aber Sie können externe Debugger verwenden, wie zum Beispiel ZeroBrane Studio. Hier ist ein Vorgeschmack darauf, wie man damit arbeitet:

```Lua
-- Dies ist ein einfaches Lua-Skript mit einem absichtlichen Fehler
local function add(a, b)
    local result = a + b -- Oh, stellen wir uns vor, wir haben vergessen, 'b' zu definieren
    return result
end

print(add(10))
```

Wenn Sie dies in einem Debugger ausführen, wird die Ausführung angehalten, wo die Dinge durcheinander geraten. Sie werden so etwas sehen:

```
lua: example.lua:3: Versuch, eine arithmetische Operation auf einem nil-Wert durchzuführen (lokale 'b')
Stapelrückverfolgung:
	example.lua:3: in Funktion 'add'
	example.lua:7: im Hauptteil
	[C]: in ?
```

Sie können Haltepunkte setzen, schrittweise durch Ihren Code gehen und Variablenwerte einsehen, um den Fehler zu finden, ohne den Verstand zu verlieren.

## Tiefer Eintauchen
Leider, die Einfachheit von Lua erstreckt sich nicht auf das Debugging. Aber keine Sorge, die Lua-Gemeinschaft hat Ihnen den Rücken frei. Werkzeuge wie ZeroBrane Studio, LuaDec und andere bieten Debugging-Fähigkeiten. Historisch gesehen existierten Debugger nicht lange, nachdem die ersten Programme sauer wurden, und gaben Entwicklern die Mittel, ihren Code zu reparieren, ohne blind herumzufummeln.

Bei Lua verlassen Sie sich oft auf externe Debugger oder bauen sie in Ihre Entwicklungsumgebung ein. ZeroBrane Studio ist zum Beispiel eine IDE, die einen Lua Debugger vollständig integriert. Es ermöglicht Ihnen, Schritt für Schritt durch den Code zu gehen, Haltepunkte zu setzen und Variablen zu beobachten. Auf der Implementierungsseite nutzen Debugger typischerweise Haken, um Haltepunkte und andere Debugging-Funktionen einzufügen.

Alternativen? Aber sicher. Gute alte `print`-Anweisungen, liebevoll als "printf-Debugging" bekannt, können manchmal den Trick tun, ohne schicke Werkzeuge.

## Siehe Auch
Um Ihre Debugging-Reise fortzusetzen, schauen Sie sich an:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Lua-Benutzer-Wiki über das Debuggen von Lua-Code: http://lua-users.org/wiki/DebuggingLuaCode
- Das `debug`-Bibliotheksreferenz im Lua-Handbuch: https://www.lua.org/manual/5.4/manual.html#6.10
