---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Ermittlung der Länge eines Strings bedeutet einfach zu bestimmen, wie viele Zeichen ein bestimmtes Wort oder Satz beinhaltet. Dies ist wichtig, da es hilft den Speicherplatz zu definieren und Textdaten zu verarbeiten.

## So Geht's:

Einen String's Länge in Lua zu finden ist einfach. Wir setzen die eingebaute Funktion `string.len()` oder die `#` Operator. Hier sind Beispiele:

```Lua
-- Beispielsatz
local text = "Hallo, Welt!"

-- Verwendung der eingebauten Funktion
print(string.len(text))

-- Verwendung des # Operators
print(#text)
```

Wenn dieses Skript ausgeführt wird, bekommt man die folgenden Ausgaben:

```
13
13
```

## Tief Tauchen

Beim Bestimmen von String Längen hat Lua immer die Längenzählerfunktion oder den `#` Operator verwendet. As in C, dieser Operator wurde aus Leistungserwägungen bevorzugt, da er wesentlich schneller auf den internen Stringlängenzähler von Lua zugreift.

Als Alternative kann man though die Funktion `utf8.len()`, dabei wird auch die Länge von Strings mit Unicode-Zeichen korrekt gezählt. Beachten Sie, dass dies eine etwas langsamere Methode ist.

Bei der internen Implementierung, Lua speichert jedes String Objekt zusammen mit dessen Länge, daher ist der Zugriff auf diese Information schnell und direkt.

## Siehe Auch

Für weitere Diskussionen, Kontext und Beispiele zu diesem Thema sind die folgenden Quellen hilfreich:

1. [Lua Handbuch - String Manipulation](https://www.lua.org/manual/5.4/manual.html#6.4)
2. [Programming in Lua - Strings](https://www.lua.org/pil/20.html)
3. [Lua-Users Wiki - Strings Tutorial](http://lua-users.org/wiki/StringLibraryTutorial)
4. [StackOverflow - Auseinandersetzung zu `#` Operator und `string.len`](https://stackoverflow.com/questions/31031045/whats-the-difference-between-the-number-sign-and-string-len-in-lua)