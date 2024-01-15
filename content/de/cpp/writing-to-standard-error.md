---
title:                "Schreiben in den Standardfehler"
html_title:           "C++: Schreiben in den Standardfehler"
simple_title:         "Schreiben in den Standardfehler"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt damit beschäftigen, Standardfehler auszugeben? Nun, manchmal reicht es einfach nicht aus, Ergebnisse auf dem Bildschirm auszugeben. Vielleicht ist es wichtig, dass bestimmte Informationen oder Fehlermeldungen direkt an die Fehlerausgabe gesendet werden.

## Wie geht das?

Um eine Meldung an die Standardfehlerausgabe zu senden, musst du die Funktion `stderr` aus der `iostream` Bibliothek verwenden. Hier ist ein Beispiel:

```C++
#include <iostream>

int main() {
    std::cerr << "Fehler im System!" << std::endl;
    return 0;
}
```

Bei der Kompilierung und Ausführung dieses Codes wird die Meldung "Fehler im System!" an die Standardfehlerausgabe gesendet.

## Tiefer Einblick

Im Gegensatz zur Standardausgabe, die hauptsächlich für die Ausgabe normaler Daten verwendet wird, ist die Standardfehlerausgabe für Fehlermeldungen und wichtige Informationen gedacht. Dadurch können Fehlermeldungen von normalen Ausgaben getrennt und leichter gefunden werden.

## Siehe auch

- [C++ Dokumentation zu Standardfehlerausgabe](https://en.cppreference.com/w/cpp/io/cerr)
- [Detaillierte Anleitung zur Fehlerbehandlung in C++](https://www.learncpp.com/cpp-tutorial/185-basic-exception-handling/)