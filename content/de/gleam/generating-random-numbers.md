---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:18.670214-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen erzeugen bedeutet einfach, nicht vorhersagbare Werte in deinem Programm zu generieren. Programmierer nutzen sie für Spiele, Simulationen, Tests und überall dort, wo Ergebnisse variieren sollen.

## Wie geht das?
In Gleam kannst du mit dem `gleam_stdlib`-Paket Zufallszahlen erzeugen. Hier siehst du wie's geht:

```gleam
import gleam_stdlib.{random_int, Int}

pub fn gib_mir_eine_zahl() -> Int {
  random_int(1, 100) // Gibt eine Zufallszahl zwischen 1 und 100 zurück
}

// Beispiel-Ausgabe: 42 (Die tatsächliche Ausgabe wird zufällig sein)
```

## Deep Dive
Zufallszahlen sind ein fundamentales Tool in der Programmierung und wurden schon seit den Anfangstagen der Computer genutzt. Ursprünglich hardwarebasiert, sind heutige Zufallszahlen oft durch softwarebasierte Pseudo-Zufallszahlengeneratoren erzeugt, die Algorithmen zur Erzeugung von Zahlenfolgen nutzen, die zufällig erscheinen. Echte Zufälligkeit erfordert physikalische Phänomene, deshalb sind sie in der Informatik als "Pseudozufallszahlen" bekannt. In Gleam sieht die Implementation dank der `gleam_stdlib` einfach aus, doch unter der Haube verwendet es erprobte Algorithmen, um diese "Pseudozufälligkeit" zu erreichen. Alternativen umfassen die Nutzung anderer Pakete oder externe Services, die zufällige Daten liefern.

## Siehe auch
- Gleam's offizielle Dokumentation zur `gleam_stdlib`: [https://hexdocs.pm/gleam_stdlib](https://hexdocs.pm/gleam_stdlib)
- Mehr über Pseudozufallszahlengeneratoren: [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- Eine Diskussion über Zufallszahlen in der Programmierung auf Stack Overflow: [https://stackoverflow.com/questions/tagged/random](https://stackoverflow.com/questions/tagged/random)