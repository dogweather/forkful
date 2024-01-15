---
title:                "Das Lesen einer Textdatei"
html_title:           "C++: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren ist es oft notwendig, Textdateien zu lesen, um Daten zu verarbeiten oder auszugeben. In diesem Artikel werde ich erklären, wie man dies mit C++ auf einfache Art und Weise tun kann.

## How To

```C++
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {

  // Öffnen einer Textdatei zum Lesen
  ifstream file("beispiel.txt");

  // Überprüfen, ob die Datei erfolgreich geöffnet wurde
  if (file.is_open()) {

    string line;

    // Lesen und Ausgabe jeder Zeile der Datei
    while (getline(file, line)) {
      cout << line << endl;
    }

    // Schließen der Datei
    file.close();
  }
  else {
    // Fehlermeldung, falls die Datei nicht geöffnet werden konnte
    cerr << "Fehler beim Öffnen der Datei" << endl;
  }

  return 0;
}
```

Beispieltextdatei "beispiel.txt":

```
Hallo!
Ich bin eine Textdatei.
Danke fürs Lesen.
```

Output:

```
Hallo!
Ich bin eine Textdatei.
Danke fürs Lesen.
```

## Deep Dive

In C++ gibt es die Header-Datei `fstream`, die Funktionen zum Lesen und Schreiben von Dateien enthält. Um eine Textdatei zum Lesen zu öffnen, verwenden wir den `ifstream` Konstruktor und geben den Dateinamen als Parameter an. Dann können wir die Datei Zeile für Zeile mit der Funktion `getline()` lesen und ausgeben. Zum Schluss ist es wichtig, die Datei mit `file.close()` zu schließen.

## Siehe auch

- [C++ Dateien einlesen](https://www.programiz.com/cpp-programming/library-function/fstream/ifstream)
- [Tutorial zum Einlesen von Dateien](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [C++ Referenz zu `fstream`](https://en.cppreference.com/w/cpp/header/fstream)