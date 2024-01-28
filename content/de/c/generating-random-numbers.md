---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-27T20:33:11.402696-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erzeugen von Zufallszahlen in C beinhaltet das Erstellen von Zahlenfolgen, die kein erkennbares Muster aufweisen und das Konzept der Zufälligkeit nachahmen. Programmierer nutzen Zufallszahlen für eine Vielzahl von Zwecken, einschließlich der Simulation von Daten, kryptografischen Anwendungen und der Spielentwicklung, was es zu einem wichtigen Aspekt der Programmierung macht.

## Wie geht das:

Um Zufallszahlen in C zu generieren, verwendet man üblicherweise die Funktion `rand()`, die in `stdlib.h` zu finden ist. Es ist jedoch entscheidend, den Zufallszahlengenerator zu initialisieren, um Variabilität in den generierten Zahlen über verschiedene Programmausführungen hinweg zu gewährleisten. Die Funktion `srand()`, die mit einem Wert initialisiert wird, oft die aktuelle Zeit, erleichtert dies.

Hier ist ein einfaches Beispiel für die Erzeugung einer Zufallszahl zwischen 0 und 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Den Zufallszahlengenerator initialisieren
    srand((unsigned) time(NULL));

    // Eine Zufallszahl zwischen 0 und 99 generieren
    int randomNumber = rand() % 100;

    // Die Zufallszahl ausgeben
    printf("Zufallszahl: %d\n", randomNumber);

    return 0;
}
```

Beispielausgabe:

```
Zufallszahl: 42
```

Es ist wichtig zu beachten, dass jede Ausführung dieses Programms eine neue Zufallszahl erzeugt, dank der Initialisierung mit der aktuellen Zeit.

## Vertiefung

Die traditionelle Methode zur Erzeugung von Zufallszahlen in C, unter Verwendung von `rand()` und `srand()`, ist nicht wirklich zufällig. Es ist pseudorandom. Das reicht für viele Anwendungen aus, fällt aber in Situationen, in denen ein hoher Grad an Zufälligkeit erforderlich ist, wie bei ernsthaften kryptografischen Anwendungen, zu kurz. Die von `rand()` generierte Sequenz wird vollständig durch den `srand()` bereitgestellten Seed bestimmt. Wenn also der Seed bekannt ist, kann die Sequenz vorhergesagt werden, was die Zufälligkeit verringert.

Historisch gesehen wurde die Funktion `rand()` aufgrund ihrer geringen Qualität der Zufälligkeit und des begrenzten Bereichs kritisiert. Moderne Alternativen umfassen die Verwendung von gerätespezifischen APIs oder externen Bibliotheken, die wahre Zufälligkeit besser annähern oder, in UNIX-ähnlichen Systemen, das Lesen von `/dev/random` oder `/dev/urandom` für kryptografische Zwecke.

Zum Beispiel die Verwendung von `/dev/urandom` in C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // /dev/urandom zum Lesen öffnen
    fp = fopen("/dev/urandom", "r");

    // Eine Zufallszahl lesen
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Die Zufallszahl ausgeben
    printf("Zufallszahl: %u\n", randomNumber);

    // Die Datei schließen
    fclose(fp);

    return 0;
}
```

Diese Methode liest direkt aus dem Entropiepool des Systems und bietet eine höhere Qualität an Zufälligkeit, die für empfindlichere Anwendungen geeignet ist. Dieser Ansatz kann jedoch Portabilitätsprobleme auf verschiedenen Plattformen haben, was ihn weniger universell als die Verwendung von `rand()` macht.

Unabhängig von der Methode ist das Verständnis der Natur der Zufälligkeit und deren Implementierung in C entscheidend für die Entwicklung von effektiven, sicheren und fesselnden Anwendungen.
