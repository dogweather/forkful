---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung kann das Generieren von Zufallszahlen dafür genutzt werden, um unvorhersehbare Ergebnisse zu erzeugen. Das ist überlebenswichtig in Bereichen wie Kryptographie, Simulationen oder bei Spielen, wo die Zufälligkeit eine unberechenbare Dynamik einbringt.

## Anleitung:

In Lua verwenden wir die Funktion `math.random()`, um Zufallszahlen zu erzeugen. Der Aufruf dieser Funktion gibt eine Gleitkommazahl zurück, die im Bereich zwischen 0 (einschließlich) und 1 (ausschließlich) liegt. Hier sehen Sie es in Aktion:

```Lua
print(math.random()) -- Gibt eine Zufallszahl zwischen 0 und 1 zurück
```

Um eine ganze Zufallszahl zu erzeugen, nehmen wir zwei Argumente in die Funktion auf. Das erste Argument definiert den Anfang des Bereichs und das zweite Argument das Ende des Bereichs.

```Lua
print(math.random(1, 10)) -- Gibt eine Zufallszahl zwischen 1 und 10 zurück
```

## Tiefgang:

In älteren Lua-Versionen mussten Programmierer die Zufallsfunktion explizit mit der Funktion `math.randomseed(os.time())` initialisieren, um sicherzustellen, dass bei jedem Durchlauf des Programms andere Zufallszahlen erzeugt werden. Von Lua 5.3 an ist dies jedoch nicht mehr erforderlich, da die Initialisierung der Zufallsfunktion automatisch erfolgt.

Es gibt andere Ansätze zur Zufallszahlengenerierung in der Programmierung, wie z.B. die Nutzung von spezialisierten Bibliotheken oder Algorithmen wie Mersenne-Twister, die eine höhere Qualität der Zufallszahlen bieten können.

Die Implementierung von `math.random()` in Lua stützt sich auf die C Standardbibliothek-Funktion `rand()`, die eine gleichmäßige Verteilung von Zufallszahlen gewährleistet.

## Siehe auch:

Für eine ausführlichere Information, schauen Sie sich bitte die offizielle Lua-Dokumentation zur `math`-Bibliothek an:  
https://www.lua.org/pil/19.2.html

Um tiefer in die Geheimnisse der Zufallszahlen und ihre Bedeutung in der Informatik einzusteigen, empfehle ich den folgenden Artikel:  
http://www.lifl.fr/~perrin/COURS/RNG/antalek.pdf