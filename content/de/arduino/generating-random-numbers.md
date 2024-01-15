---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist eine nützliche Fähigkeit in der Welt des Programmierens, da es viele Anwendungsfälle gibt, in denen wir zufällige Werte benötigen. Zum Beispiel können wir sie verwenden, um zufällige Entscheidungen zu treffen, Spiele zu erstellen oder Verschlüsselung zu verbessern.

## So geht's

Um zufällige Zahlen in Arduino zu generieren, können wir die `random()` Funktion verwenden. Wir müssen jedoch zuerst darauf achten, dass der Zufallszahlengenerator initialisiert wird, indem wir `randomSeed()` verwenden. Schauen wir uns ein Beispiel an:

```Arduino
// Initialisieren des Zufallszahlengenerators
randomSeed(analogRead(A0));

// Generieren einer zufälligen Ganzzahl zwischen 0 und 10
int randomNumber = random(10); 

// Ausgabe der Zufallszahl auf dem seriellen Monitor
Serial.println(randomNumber); 
```

In diesem Beispiel haben wir den Zufallszahlengenerator mit einer analogen Messung initialisiert, um eine unvorhersehbare Ausgangsbasis zu schaffen. Wir haben dann eine zufällige Ganzzahl zwischen 0 und 10 generiert und sie auf dem seriellen Monitor ausgegeben.

Neben der `random()` Funktion gibt es auch andere Funktionen wie `random(min, max)` zum Generieren von Zufallszahlen in einem bestimmten Bereich, `randomSeed(seed)` zum Festlegen einer benutzerdefinierten Startzahl und `randomize()` zum Aktualisieren des Zufallszahlengenerators basierend auf externen Einflüssen.

## Tiefentauchen

Hinter den Kulissen verwendet der Zufallszahlengenerator in Arduino den linearen kongruenten Generator (LCG) Algorithmus. Dieser Algorithmus ist relativ einfach und schnell, aber er kann bei bestimmten Anwendungsfällen unzuverlässige Ergebnisse liefern, da er periodische Muster aufweisen kann.

Um dieses Problem zu umgehen, gibt es in Arduino auch die `randomBytes()` Funktion, die auf einer Pseudozufallszahlengenerator-Bibliothek basiert und eine bessere Qualität der Zufallszahlen bietet. Dies ist besonders wichtig, wenn es um kryptografische Anwendungen geht.

## Siehe auch

- [Arduino - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino - randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Arduino - randomBytes()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randombytes/)