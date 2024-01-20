---
title:                "Das Schreiben einer Textdatei"
html_title:           "C++: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Schreiben einer Textdatei ist im Grunde genommen das Verfassen von Text in einer Datei. Programmierer tun dies, um Informationen zu speichern und sie später wieder abrufen zu können, anstatt sie jedes Mal neu eingeben zu müssen.

# Wie?

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  // Erstellen einer Textdatei
  ofstream file;
  file.open("textdatei.txt");

  // Schreiben von Text in die Datei
  file << "Dies ist ein Beispieltext.";

  // Schließen der Datei
  file.close();
  
  // Öffnen der Datei und Ausgabe
  ifstream read_file("textdatei.txt");
  string text;
  while (getline(read_file, text)) {
      cout << text << endl;
  }
  read_file.close();

  return 0;
}
```

Ausgabe:

Dies ist ein Beispieltext.

# Tiefere Einblicke

Das Schreiben von Textdateien gibt es schon seit Beginn der Programmierung. In der Vergangenheit wurden dafür sogenannte "Druckausgaben" verwendet, die auf einem Zettel oder einer Karte angebracht wurden. Heutzutage gibt es auch alternative Möglichkeiten, um Daten zu speichern, wie zum Beispiel Datenbanken.

Beim Schreiben einer Textdatei müssen Programme bestimmte Regeln und Formatierungen einhalten, um die Datei korrekt zu erstellen. Dazu gehören auch die Verwendung von Zeilenumbrüchen und die richtige Codierung.

# Siehe auch

- [C++ ifstream & ofstream](https://www.geeksforgeeks.org/basic-input-output-c/)
- [Verschiedene Dateiformate](https://www.digitalocean.com/pricing/#:~:text=Welcome-,to%20the%20World%2Drenowned%20Digital%20Ocean%20Network,-With%2012%20data)