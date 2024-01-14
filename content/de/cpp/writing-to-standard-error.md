---
title:                "C++: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Programmierer von Zeit zu Zeit den Standardfehler (auch bekannt als "Standarderror" oder "stderr") in C++ nutzen. Einer der Hauptgründe ist, dass es ein effektives Werkzeug ist, um Fehlermeldungen und Warnungen während der Entwicklung zu debuggen. Das Senden von Text auf den Standardfehler ist auch nützlich, um bestimmte Informationen auf der Konsole zu protokollieren, ohne dass der Hauptprogrammablauf unterbrochen wird.

## Wie geht das

Um Text auf den Standardfehler auszugeben, können Sie die Funktion `std::cerr` aus der Header-Datei `<iostream>` verwenden. Hier ist ein Beispiel:

```C++
#include <iostream>

int main() {
    std::cerr << "Dies wird auf dem Standardfehler ausgegeben." << std::endl;
    return 0;
}
```
Das Ergebnis dieses Codes sollte folgendes auf der Konsole ausgeben:

`Dies wird auf dem Standardfehler ausgegeben.`

Um effektiv mit dem Standardfehler zu arbeiten, ist es wichtig, sicherzustellen, dass Sie die richtigen Daten- und Fehlerbehandlungstechniken implementiert haben. Hier sind einige Tipps, die Sie beachten sollten:

- Verwenden Sie `std::cerr` für Fehlermeldungen und Warnungen, die während der Entwicklung auf Fehler hinweisen können, während `std::cout` für die Ausgabe von normalen Ergebnissen verwendet werden sollte.
- Vergessen Sie nicht, `std::endl` am Ende Ihres Textes auf dem Standardfehler auszugeben, um einen Zeilenumbruch hinzuzufügen.
- Stellen Sie sicher, dass Sie den entsprechenden Header-Datei-Code `#include <iostream>` eingeben, bevor Sie den Standardfehler verwenden.

## Tiefer Einblick

Der Standardfehler ist ein Objekt vom Typ `std::ostream`, genau wie `std::cout` und `std::cin`. Das bedeutet, dass Sie ihn ähnlich wie die anderen Streams behandeln können. Sie können beispielsweise den Operator `<<` verwenden, um Text auf den Standardfehler auszugeben, genau wie Sie es mit `std::cout` tun würden. Sie können auch die Methoden `.put()` und `.write()` verwenden, um einzelne Zeichen bzw. eine Sequenz von Zeichen auf den Standardfehler auszugeben.

Es gibt auch viele erweiterte Funktionen, die Sie mit dem Standardfehler tun können, einschließlich der Verwendung verschiedener Manupulationsbefehle, die in der `<iomanip>` Header-Datei definiert sind.

## Siehe auch

- [C++: Einführung in die Standardbibliothek](https://www.cplusplus.com/doc/tutorial/iostream/)
- [C++: Formatierung von Ausgabe mit `std::setw` und `std::setprecision`](https://www.cplusplus.com/reference/iomanip/)