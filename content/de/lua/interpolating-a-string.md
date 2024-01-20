---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Interpolation von Zeichenketten ermöglicht das Einsetzen von Variablen oder Ausdrücken direkt in Zeichenketten. Programmierer verwenden dies, um leserlichen und wartbaren Code zu schreiben.

## Anleitung:

In Lua verwenden wir die `string.format`-Funktion zur Zeichenketteninterpolation. Hier sind ein paar Beispiele:

```Lua
name = "John"
print(string.format("Hallo %s", name))   -- Ausgabe: Hallo John

zahl = 5
print(string.format("Ich habe %d Äpfel", zahl))   -- Ausgabe: Ich habe 5 Äpfel

preis = 9.99
print(string.format("Das kostet %.2f Euro", preis)) -- Ausgabe: Das kostet 9.99 Euro

```

## Tiefere Informationen:

Zeichenketteninterpolation geht auf die 60er Jahre und die Assemblerprogrammierung zurück. In Lua, das keine integrierte Unterstützung für Zeichenketteninterpolation hat, verwenden wir die `string.format`-Funktion, basierend auf dem C Sprachstil.

Als Alternativen zur Interpolation in Lua gelten konkatenierte Zeichenketten oder die Verwendung externer Bibliotheken wie `f-Strings`.

In Bezug auf die Implementierungsdetails verwendet Lua den Formatierungstyp spezifizierer (wie `%s`, `%d`, `%.2f`), um den Wert und den Typ der einzufügenden Variable anzugeben.

## Siehe dazu:

Um Ihre Kenntnisse in Lua zu vertiefen, finden Sie hier eine Liste von nützlichen Ressourcen.

- [Lua-Users-Wiki: Strings Tutorial](http://lua-users.org/wiki/StringsTutorial)
- [Lua: Programming: gsub](https://www.lua.org/pil/20.2.html)
- [Programming in Lua: The String Library](https://www.lua.org/pil/20.html)