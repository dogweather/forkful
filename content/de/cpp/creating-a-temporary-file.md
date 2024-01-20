---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Temporäre Dateien in C++: Was, Warum und Wie?

## Was & Warum?

Die Erstellung einer temporären Datei ist ein alltäglicher Prozess beim Programmieren, der einen kurzfristigen Speicher für Daten während des Programmablaufs bereitstellt. Programmierer machen dies, um Daten zwischenspeichern zu können, die nicht dauerhaft gespeichert werden müssen, oder um Speicherplatz auf effiziente Weise zu verwalten.

## Wie zu:

Für die Erstellung einer temporären Datei in C++ können wir die Funktionen `tmpnam()` oder `tempnam()`. Hier ist ein einfaches Beispiel:

```C++
#include <cstdio>

int main() {
    char tmpname[L_tmpnam];
    char *filename = tmpnam(tmpname);

    printf("Temporäre Dateiname: %s\n", filename);

    return 0;
}
```

Wenn Sie dieses Programm ausführen, wird ein einzigartiger Dateiname ausgedruckt, der als temporäre Datei verwendet werden kann.

## Vertiefte Kenntnisse:

In den ersten Tagen von UNIX und C wurden temporäre Dateien über die Funktionen `tmpfile()`oder `mkstemp()` erstellt. Diese Funktionen sind jedoch in C++ nicht mehr allgemein in Gebrauch.

Es gibt auch Alternativen zur Erstellung temporärer Dateien in C++, wie die Klasse `tempfile` in einigen Bibliotheken. Allerdings sind Funktionen wie `tmpnam()` in der Standardbibliothek und daher allgemein verfügbar.

Bei der Verwendung von `tmpnam()` wird eine Sequenz von Buchstaben und Zahlen erzeugt, die einen einzigartigen Dateinamen ergeben. Der exakte Algorithmus ist jedoch implementierungsspezifisch und kann von einer C++ Umgebung zur anderen variieren.

## Siehe auch:

1. C++ Referenz für `tmpnam()`: http://www.cplusplus.com/reference/cstdio/tmpnam/
2. C++ Referenz für `tempnam()`: http://www.cplusplus.com/reference/cstdio/tempnam/
3. Alternativen zur Erstellung temporärer Dateien: http://www.boost.org/doc/libs/1_59_0/libs/io/doc/classes/iostreams/temp_file.html
4. Geschichte von temporären Dateien in UNIX und C: https://en.wikipedia.org/wiki/Tmpfile