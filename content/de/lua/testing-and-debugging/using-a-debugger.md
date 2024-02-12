---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T03:50:09.085077-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger ist ein Werkzeug, das Ihnen erlaubt, die Ausführung eines Programms zu inspizieren und zu kontrollieren. Dadurch wird es einfach, genau zu bestimmen, wo die Dinge schiefgehen. Programmierer nutzen Debugger, um Bugs zu beseitigen, den Codefluss zu verstehen und um sicherzustellen, dass ihr Code so sauber wie eine Pfeife ist.

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
