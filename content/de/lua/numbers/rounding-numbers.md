---
date: 2024-01-26 03:45:50.354192-07:00
description: "Das Runden von Zahlen bedeutet, sie auf die n\xE4chstliegende ganze\
  \ Zahl oder eine bestimmte Dezimalstelle anzupassen. Es ist ein Grundelement in\
  \ der\u2026"
lastmod: '2024-03-13T22:44:54.010015-06:00'
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen bedeutet, sie auf die n\xE4chstliegende ganze Zahl\
  \ oder eine bestimmte Dezimalstelle anzupassen."
title: Zahlen runden
weight: 13
---

## Was & Warum?
Das Runden von Zahlen bedeutet, sie auf die nächstliegende ganze Zahl oder eine bestimmte Dezimalstelle anzupassen. Es ist ein Grundelement in der Programmierung, um die Komplexität zu reduzieren, die Leistung zu steigern und für Zeiten, in denen Genauigkeit über einen bestimmten Punkt hinaus keinen Wert hinzufügt.

## Wie geht das:
```lua
-- Grundlegendes Runden in Lua ist nicht standardmäßig enthalten, aber Sie können eine Funktion definieren:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Um auf eine bestimmte Dezimalstelle zu runden:
function round(num, dezimalstellen)
    local mult = 10^(dezimalstellen or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Tiefer eintauchen
Lua enthält standardmäßig keine Rundungsfunktion, anders als einige andere Sprachen. Historisch gesehen müssen Sie Ihre eigene schreiben oder eine Bibliothek von Drittanbietern verwenden. Gängige Workarounds stützen sich auf `math.floor()` für das Abrunden und `math.ceil()` für das Aufrunden, gekoppelt mit dem Hinzufügen oder Abziehen von 0,5 davor, abhängig vom Vorzeichen der Zahl.

Alternativen zur Erstellung Ihrer eigenen Funktion umfassen Bibliotheken wie "lua-users wiki" oder "Penlight". Jede hat ihre Vorteile und Kompromisse, wie zusätzliche Funktionen oder mehr Overhead.

Intern arbeiten diese Funktionen normalerweise, indem sie die Art und Weise ausnutzen, wie Computer Gleitkommazahlen speichern. Das Hinzufügen von 0,5 zu einer positiven Float-Zahl, die Sie runden möchten, wird sie über den Schwellenwert des nächsten ganzzahligen Werts drücken, so dass, wenn Sie `math.floor()` anwenden, es auf die nächstliegende ganze Zahl abrundet.

## Siehe auch
- [Lua 5.4 Referenzhandbuch: Die mathematischen Funktionen](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Lua-Bibliotheken: Mathematik](https://github.com/lunarmodules/Penlight)
