---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Eine Einführung in das Einlesen von Textdateien in C++

## Was & Warum?

Einlesen von Textdateien in C++ ist, den Inhalt einer Textdatei in deinem Programm zu lesen und zu verwenden. Als Programmierer tun wir dies, um Daten zu verarbeiten oder als Eingabe für unsere Algorithmen zu verwenden.

## Wie man:

Zur Veranschaulichung, wie man eine Textdatei liest, nutzt man den `fstream` Bibliothek in C++. Hier ist ein einfacher Code zum Einlesen einer Textdatei.

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("beispiel.txt");
    std::string str;
    while (std::getline(file, str)) {
        std::cout << str << "\n";
    }
    return 0;
}
```

Dieser Code wird `beispiel.txt` öffnen und jede Zeile einzeln lesen und auf dem Bildschirm ausgeben.

Wenn Ihre Textdatei beispielsweise folgendes enthält:

```
Hallo, Welt!
Ich lerne C++.
```

Dann wird Ihre Ausgabe genau dasselbe sein.

## Tiefgang

Historisch gesehen begann die Praxis des Einlesens von Textdateien in den frühen Tagen der Computerprogrammierung, wo es eine der wenigen Möglichkeiten war, Informationen in ein Computerprogramm zu bekommen.

Alternativ könnten wir 'mmap', 'fread' oder 'read' in C++ verwenden, um Textdateien zu lesen, diese Methoden sind jedoch komplexer und eher auf low-level Operationen ausgerichtet.

Die Implementierung des Einlesens einer Datei in C++ erfordert das Verständnis von Input/Output-Streams und File-Handling. Die `fstream` Bibliothek bietet die `ifstream` Klasse, die zum Lesen von Dateien verwendet wird. Mit dem `getline()` Befehl können wir jede Zeile der Datei lesen.

## Siehe Auch

1. [Offizielle Dokumentation zu fstream](http://www.cplusplus.com/reference/fstream/)
2. [fstream Tutorial](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
3. [Beispiel für die Verwendung von ifstream](https://www.geeksforgeeks.org/readwrite-class-objects-fromto-file-c/)