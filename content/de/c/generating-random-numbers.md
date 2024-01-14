---
title:                "C: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung, insbesondere im Bereich der Spieleentwicklung und der Verschlüsselung. Durch die Verwendung von Zufallszahlen können Programme Abwechslung erzeugen und vorhersehbare Ergebnisse vermeiden.

## Wie man es macht

Das Generieren von Zufallszahlen in C ist relativ einfach. Im Folgenden werden wir uns ein paar Beispiele ansehen, wie man mit Hilfe der Standardbibliothek von C Zufallszahlen generieren kann.

```C
// Zunächst müssen wir die Header-Datei "stdlib.h" einbinden
#include <stdlib.h>

// Um eine Zufallszahl zwischen 0 und RAND_MAX zu generieren, verwenden wir die Funktion rand()
int random_number = rand(); 

// Wenn wir eine bestimmte Spanne von Zahlen haben wollen, können wir die Funktion modulo (%) verwenden
int random_number_between_0_and_9 = rand() % 10; // gibt eine Zufallszahl zwischen 0 und 9 zurück

// Wir können auch Zufallszahlen zwischen zwei bestimmten Zahlen generieren
int random_number_between_20_and_30 = rand() % 11 + 20; // gibt eine Zufallszahl zwischen 20 und 30 zurück
```

Wenn wir die obigen Code-Beispiele ausführen, erhalten wir jedes Mal eine andere Zufallszahl.

## Tiefentauchen

Das Generieren von Zufallszahlen mag auf den ersten Blick einfach aussehen, jedoch gibt es einige wichtige Punkte zu beachten. Zum einen ist die Funktion rand() nicht wirklich zufällig, sondern basiert auf einem Algorithmus, der bei jedem Programmstart die gleichen Zufallszahlen produziert. Deshalb sollten wir die Funktion srand() verwenden, um eine sogenannte "Seed"-Nummer zu setzen, die den Startpunkt für die Zufallszahlengenerierung definiert.

```C
#include <stdlib.h>
#include <time.h>

srand(time(NULL)); // initialisiert den Seed mit der aktuellen Uhrzeit
```

Zum anderen gibt es verschiedene Arten von Zufallszahlen, wie zum Beispiel Pseudozufallszahlen und kryptografisch sichere Zufallszahlen. Je nach Anwendungsbereich müssen wir die geeignete Methode zur Generierung von Zufallszahlen wählen.

## Siehe auch

- Weitere Informationen zur Generierung von Zufallszahlen in C: [https://www.linuxprogrammingblog.com/code-examples/how-to-use-random-and-rand-in-cc](https://www.linuxprogrammingblog.com/code-examples/how-to-use-random-and-rand-in-cc)
- Eine detaillierte Erläuterung der verschiedenen Arten von Zufallszahlen und deren Einsatzgebiete: [https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- Eine Einführung in die Verwendung von Zufallszahlen in der Spieleentwicklung: [https://noobtuts.com/cpp/12-generate-random-geometry](https://noobtuts.com/cpp/12-generate-random-geometry)