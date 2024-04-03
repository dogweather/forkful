---
date: 2024-01-20 17:38:56.052733-07:00
description: 'How to: In Lua ist die Umwandlung eines Strings in Kleinbuchstaben einfach
  und geradlinig mit der Funktion `string.lower()`. Hier ein Beispiel.'
lastmod: '2024-03-13T22:44:54.002218-06:00'
model: gpt-4-1106-preview
summary: In Lua ist die Umwandlung eines Strings in Kleinbuchstaben einfach und geradlinig
  mit der Funktion `string.lower()`.
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## How to:
In Lua ist die Umwandlung eines Strings in Kleinbuchstaben einfach und geradlinig mit der Funktion `string.lower()`. Hier ein Beispiel:

```Lua
local myString = "Hallo Welt!"
local lowerCaseString = string.lower(myString)
print(lowerCaseString)  -- Ausgabe: "hallo welt!"
```

## Deep Dive
Lua nutzt für die Umwandlung in Kleinbuchstaben die C-Funktion `tolower` aus der Standardbibliothek. Diese Standardfunktion beachtet nicht die sprachspezifischen Regeln der Groß-/Kleinschreibung, sondern konvertiert rein auf Basis der ASCII-Werte. 

In der Geschichte von Programmiersprachen war die Groß-/Kleinschreibung häufig ein Stolperstein beim Sortieren oder Vergleichen von Text, insbesondere in Sprachen, die eine lexikalische Ordnung anwenden. Vor Lua und anderen modernen Sprachen mussten Entwickler ihre eigenen Funktionen schreiben, um diese Konversion durchzuführen.

Neben `string.lower()` gibt es in Lua keine eingebauten Alternativen für die Kleinbuchstaben-Umwandlung. In anderen Sprachen finden sich häufig Methoden wie `toLocaleLowerCase()`, die regionale Regeln beachten. Für solch eine Funktionalität in Lua müsste man auf zusätzliche Bibliotheken oder eigens implementierte Funktionen zurückgreifen.

## See Also
Weitere Informationen zu String-Operationen in Lua findest du in der offiziellen Lua-Dokumentation:

- Lua 5.4 Referenzhandbuch: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua-Users Wiki über Strings: http://lua-users.org/wiki/StringLibraryTutorial

Für internationale Groß-/Kleinschreibung könntest du folgende Bibliotheken betrachten:

- Lua Unicode Library: https://github.com/starwing/luautf8
- ICU (International Components for Unicode) Lua bindings: https://github.com/deepakjois/luacxx-icu
