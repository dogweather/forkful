---
title:                "Schreiben auf den Standardfehler"
html_title:           "C++: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben in den Standardfehler ist ein Verfahren, bei dem Fehler und Warnungen während des Programmablaufs an eine spezielle Ausgabequelle gesendet werden. Programmierer verwenden diese Methode, um ungewöhnliche oder unerwartete Ereignisse während der Ausführung ihres Codes zu identifizieren und zu beheben.

## Wie geht's?

Um den Standardfehler in C++ zu schreiben, gibt es zwei Möglichkeiten. Die erste ist die Verwendung der Funktion std::cerr, die Teil der Standardbibliothek ist. Hier ist ein Beispielcode:

```C++
#include <iostream>
int main() 
{
    std::cerr << "Dies ist eine Fehlermeldung!" << std::endl;
    return 0;
}
```

Die oben genannte Methode verwendet die Stream-Datenstruktur, um Fehlermeldungen an den Standardfehler zu senden. Eine andere Möglichkeit ist die Verwendung der Funktion fprintf aus der C-Standardbibliothek. Hier ist ein Beispielcode:

```C++
#include <stdio.h>
int main() 
{
    fprintf(stderr, "Dies ist eine Fehlermeldung! \n");
    return 0;
}
```

Beide Methoden erzeugen die gleiche Ausgabe:

```
Dies ist eine Fehlermeldung!
```

## Tiefer eintauchen

Das Schreiben in den Standardfehler ist seit den Anfängen der Programmierung eine bewährte Methode, um Fehler bereitzustellen. Es ist eine zuverlässige Methode, um während der Ausführung Fehler zu identifizieren und zu beheben. Alternativ können Programmierer auch Logdateien oder Debugging-Tools verwenden, um Fehler zu finden, aber das Schreiben in den Standardfehler ist in der Regel schneller und einfacher.

In C++ verwenden Programmierer normalerweise std::cerr anstelle von std::cout für die Fehlerausgabe, da die beiden Streams manchmal zwischensprechen können, wenn sie gleichzeitig verwendet werden. Das heißt, wenn Sie std::cerr verwenden, erhalten Sie die Fehlerausgabe in der Reihenfolge, in der sie auftritt.

In der Regel sollten Programmierer bei der Verwendung von std::cerr und fprintf ein Leerzeichen und ein Zeilenumbruch nach der Fehlermeldung hinzufügen. Dies ist wichtig, damit die Ausgabe lesbar bleibt und das Zeilenumbruchzeichen verhindert, dass die folgenden Ausgaben mit der Fehlermeldung vermischt werden.

## Siehe auch

- [C++ Dokumentation zum Schreiben in den Standardfehler](https://en.cppreference.com/w/cpp/io/cerr)
- [C-Dokumentation zum Schreiben in den Standardfehler](https://en.cppreference.com/w/c/io/fprintf)