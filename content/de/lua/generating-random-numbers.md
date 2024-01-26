---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:32.947425-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen erzeugen bedeutet, nicht vorhersagbare Werte zu generieren, die bei jedem Lauf variieren. Programmierer nutzen sie für Spiele, Simulationen und Tests, um Zufälligkeit und Nachahmung realer Bedingungen zu erreichen.

## Wie geht das:
Für Zufallszahlen in Lua ist `math.random` Dein Freund. Zuerst seedest Du den Zufallsgenerator, damit die Zahlen echt zufällig sind – idealerweise mit der aktuellen Zeit.

```lua
math.randomseed(os.time())

-- Zufallszahl zwischen 1 und 10
local zufallszahl1 = math.random(1, 10)
print(zufallszahl1) -- z.B. 6

-- Zufallszahl zwischen 0 und 1
local zufallszahl2 = math.random()
print(zufallszahl2) -- z.B. 0.4458
```

## Deep Dive
In den frühen Tagen verwendete man simple Algorithmen für Zufallszahlen, die oft vorhersehbar waren – schlecht für Sicherheit und Spiele. Heute sind die Algorithmen komplexer, sorgen für bessere Zufälligkeit. Lua nutzt eine Variante des Mersenne Twister, der als sehr gut gilt.

Alternativen? Andere Sprachen bieten ähnliche Funktionen – `rand() in C`, `random.randint` in Python, `Math.random()` in JavaScript. Du kannst auch externe Bibliotheken nutzen, falls Du spezielle Anforderungen hast.

Eine Implementierungsdetails: Seede den Zufallsgenerator einmal am Anfang, nicht bei jeder Zahlenziehung – das verhindert Muster und erhöht die Zufälligkeit.

## Siehe auch
- Lua-Referenz zur `math`-Bibliothek: https://www.lua.org/manual/5.4/manual.html#6.7
- Mersenne Twister-Algorithmus: https://en.wikipedia.org/wiki/Mersenne_Twister
- Eine Diskussion über Zufälligkeit in Computerspielen auf StackExchange: https://gamedev.stackexchange.com/questions/162976/how-do-random-number-generators-work
