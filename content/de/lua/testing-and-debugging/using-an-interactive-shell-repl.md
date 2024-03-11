---
date: 2024-01-26 04:15:54.755401-07:00
description: "REPL steht f\xFCr Read-Eval-Print Loop, eine interaktive Umgebung, in\
  \ der man schnell Code testen kann. Programmierer nutzen sie zum Experimentieren,\u2026"
lastmod: '2024-03-11T00:14:27.915031-06:00'
model: gpt-4-0125-preview
summary: "REPL steht f\xFCr Read-Eval-Print Loop, eine interaktive Umgebung, in der\
  \ man schnell Code testen kann. Programmierer nutzen sie zum Experimentieren,\u2026"
title: Nutzung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?
REPL steht für Read-Eval-Print Loop, eine interaktive Umgebung, in der man schnell Code testen kann. Programmierer nutzen sie zum Experimentieren, Debuggen und zum Erlernen der Eigenheiten einer Sprache.

## Wie geht das:
Um in Lua's REPL zu springen, geben Sie einfach `lua` in Ihrem Terminal ein. Hier ist ein Beispiel einer Sitzung:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1   apple
2   banana
3   cherry
4   date
>
```
In der Sitzung deklarieren wir eine Variable, führen grundlegende Arithmetik durch, manipulieren eine Tabelle und durchlaufen ihre Elemente.

## Vertiefung
Die leichtgewichtige Natur von Lua macht sein REPL ideal für Prototypen. Es gibt es seit den Anfängen von Lua Anfang der 1990er Jahre, inspiriert von früheren interaktiven Shells für Sprachen wie Lisp. Alternativen in anderen Sprachen umfassen `irb` für Ruby und `python` für Python, jede mit ihrem eigenen Satz an Funktionen. Lua's REPL ist minimalistisch; daher könnte es fortgeschrittene Funktionen, die in anderen vorkommen, wie komplexe Debugging-Tools, fehlen. Für ein umfangreicheres Erlebnis bieten Tools wie ZeroBrane Studio oder LuaDist's LuaRocks mehr als das grundlegende REPL.

## Siehe auch
- [Lua 5.4 Referenzhandbuch - Der eigenständige Lua-Interpreter](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
