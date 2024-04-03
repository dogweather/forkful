---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:01.406776-07:00
description: "Wie: In C k\xF6nnen Zufallszahlen mit der Funktion `rand()` generiert\
  \ werden, die Teil der C-Standardbibliothek `<stdlib.h>` ist. Standardm\xE4\xDF\
  ig erzeugt\u2026"
lastmod: '2024-03-13T22:44:54.347736-06:00'
model: gpt-4-0125-preview
summary: "In C k\xF6nnen Zufallszahlen mit der Funktion `rand()` generiert werden,\
  \ die Teil der C-Standardbibliothek `<stdlib.h>` ist."
title: Zufallszahlen generieren
weight: 12
---

## Wie:
In C können Zufallszahlen mit der Funktion `rand()` generiert werden, die Teil der C-Standardbibliothek `<stdlib.h>` ist. Standardmäßig erzeugt `rand()` Pseudozufallszahlen im Bereich von 0 bis `RAND_MAX` (eine in `<stdlib.h>` definierte Konstante). Für mehr Kontrolle über den Bereich können Programmierer die Ausgabe von `rand()` manipulieren.

Hier ist ein einfaches Beispiel für die Generierung einer Zufallszahl zwischen 0 und 99:

```c
#include <stdio.h>
#include <stdlib.h> // Für rand() und srand()
#include <time.h>   // Für time()

int main() {
    // Den Zufallszahlengenerator initialisieren
    srand((unsigned) time(NULL));

    // Eine Zufallszahl zwischen 0 und 99 generieren
    int randomNumber = rand() % 100;

    printf("Zufallszahl: %d\n", randomNumber);

    return 0;
}
```

Die Ausgabe könnte bei jedem Ausführen dieses Programms variieren:

```
Zufallszahl: 42
```
Um Zufallszahlen in einem anderen Bereich zu generieren, können Sie den Modulo-Operator (`%`) entsprechend anpassen. Beispielsweise erzeugt `rand() % 10` Zahlen von 0 bis 9.

Es ist wichtig zu beachten, dass das Initialisieren des Pseudozufallszahlengenerators (`srand()`-Aufruf) mit der aktuellen Zeit (`time(NULL)`) unterschiedliche Folgen von Zufallszahlen bei Programmausführungen sicherstellt. Ohne Initialisierung (`srand()`) würde `rand()` bei jedem Ausführen des Programms dieselbe Zahlenfolge erzeugen.

## Vertiefung
Die Funktion `rand()` und ihr Initialisierungspendant `srand()` sind seit Jahrzehnten Teil der C-Standardbibliothek. Sie basieren auf Algorithmen, die Sequenzen von Zahlen generieren, die nur scheinbar zufällig sind – daher der Begriff "Pseudozufall". Der zugrunde liegende Algorithmus in `rand()` ist typischerweise ein linearer Kongruenzgenerator (LCG).

Obwohl `rand()` und `srand()` für viele Anwendungen ausreichen, sind ihre Einschränkungen, insbesondere bezüglich der Qualität der Zufälligkeit und potenziellen Vorhersagbarkeit, bekannt. Für Anwendungen, die hochwertige Zufälligkeit erfordern, wie kryptografische Operationen, sollten Alternativen wie `/dev/random` oder `/dev/urandom` (auf Unix-ähnlichen Systemen) oder APIs, die von kryptografischen Bibliotheken bereitgestellt werden, in Betracht gezogen werden.

Mit der Einführung von C11 umfasste die ISO-C-Norm eine neue Kopfzeile, `<stdatomic.h>`, die eine verfeinerte Kontrolle für gleichzeitige Operationen bietet, die jedoch nicht direkt die Zufälligkeit betrifft. Für echte Zufälligkeit in C wenden sich Entwickler oft an plattformspezifische oder externe Bibliotheken, die bessere Algorithmen anbieten oder Hardware-Entropiequellen nutzen.

Denken Sie daran, dass `rand()` zwar ein einfaches und zugängliches Mittel zur Erzeugung von Pseudozufallszahlen darstellt, seine Verwendung in modernen Anwendungen jedoch durch die Qualität und Vorhersagbarkeit seiner Ausgabe begrenzt ist. Wenn robustere Lösungen erforderlich sind, insbesondere für sicherheitsbewusste Anwendungen, wird das Erkunden über die Standardbibliothek hinaus dringend empfohlen.
