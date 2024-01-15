---
title:                "Eine temporäre Datei erstellen."
html_title:           "C++: Eine temporäre Datei erstellen."
simple_title:         "Eine temporäre Datei erstellen."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man überhaupt temporäre Dateien in C++ erstellen? Die Antwort ist einfach: Manchmal müssen wir während der Laufzeit unseres Programms Daten zwischenspeichern oder temporäre Ergebnisse generieren, die wir später nicht mehr benötigen. Hier kommen temporäre Dateien ins Spiel, die effektiv und sicher diese Aufgabe erfüllen können.

## How To
Um eine temporäre Datei in C++ zu erstellen, müssen wir einige grundlegende Schritte befolgen. Zunächst müssen wir die entsprechenden Header-Dateien einbinden:
```C++
#include <cstdio>
#include <cstdlib>
#include <iostream>
```

Als nächstes müssen wir einen Dateinamen für unsere temporäre Datei generieren. Dies kann mit der Funktion `tmpnam` geschehen, die einen eindeutigen Dateinamen innerhalb eines bestimmten Verzeichnisses erstellt:
```C++
char filename[L_tmpnam];
tmpnam(filename);
```

Als nächstes öffnen wir die temporäre Datei mit der `fopen` Funktion und überprüfen, ob sie erfolgreich geöffnet wurde:
```C++
FILE* file = fopen(filename, "w+");
if (file == NULL) {
    std::cerr << "Fehler beim Öffnen der temporären Datei\n";
    exit(EXIT_FAILURE);
}
```

An diesem Punkt können wir unsere temporäre Datei genau wie jede andere Datei in C++ verwenden. Wir können Daten schreiben, lesen oder bearbeiten. Zum Beispiel können wir einen Satz in die Datei schreiben und ihn dann aus der Datei lesen und auf der Konsole ausgeben:
```C++
fprintf(file, "Hallo Welt!");
rewind(file); // Dateizeiger zurücksetzen
char sentence[100];
fgets(sentence, 100, file);
std::cout << sentence; // Ausgabe: Hallo Welt!
```

Schließlich müssen wir die temporäre Datei wieder schließen und löschen, sobald wir sie nicht mehr benötigen:
```C++
fclose(file); // Datei schließen
remove(filename); // Datei löschen
```

## Deep Dive
Die `tmpnam` Funktion generiert einen eindeutigen Dateinamen, aber sie garantiert nicht, dass die Datei tatsächlich erstellt werden kann. Um sicherzustellen, dass die Datei erstellt werden kann, sollten wir `mkstemp` verwenden, das einen Dateideskriptor zurückgibt, der bereits auf eine offene Datei zeigt. Außerdem können wir mit `mkstemp` auch angeben, wo die temporäre Datei erstellt werden soll, anstatt sie in einem Standardverzeichnis zu erstellen.

Eine weitere wichtige Sache, die wir beachten müssen, ist die Sicherheit. Wir sollten immer überprüfen, ob die Funktionen `fopen` und `remove` erfolgreich ausgeführt wurden, um mögliche Sicherheitslücken zu vermeiden. Außerdem sollten wir sicherstellen, dass unsere temporäre Datei vertrauliche Informationen nicht preisgibt, wenn sie später von anderen Programmen oder Benutzern geöffnet wird.

## Siehe auch
- [cppreference - tmpnam](https://en.cppreference.com/w/cpp/io/c/tmpnam)
- [cppreference - mkstemp](https://en.cppreference.com/w/cpp/io/c/mkstemp)
- [cppreference - fopen](https://en.cppreference.com/w/cpp/io/c/fopen)
- [cppreference - remove](https://en.cppreference.com/w/cpp/io/c/remove)