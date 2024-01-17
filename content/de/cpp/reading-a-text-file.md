---
title:                "Ein Textdokument lesen"
html_title:           "C++: Ein Textdokument lesen"
simple_title:         "Ein Textdokument lesen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien sind einfache Dateien, die Text als fortlaufende Zeichenfolge speichern. Programmierer verwenden Textdateien, um Daten zu speichern oder als Eingabequelle für ihre Programme zu nutzen.

## Wie geht's:
Die C++ Standardbibliothek enthält die Funktion "ifstream", die das Lesen von Textdateien ermöglicht. Sie muss in den Header `<fstream>` eingebunden werden. Mit dieser Funktion können Sie eine Verbindung zu einer Textdatei herstellen und dann Zeile für Zeile auslesen.
```C++
#include <fstream> 
#include <iostream> 
using namespace std; 
int main() { 
   ifstream datei("textdatei.txt"); // Verbindung zur Datei herstellen 
   if (!datei) { 
     cout << "Fehler beim Öffnen der Datei!" << endl; 
   } else { 
     string line; 
     while (!datei.eof()) { // solange die Datei nicht zu Ende ist 
       getline(datei, line); // Zeile auslesen und in "line" speichern 
       cout << line << endl; // Zeile ausgeben 
     } 
   } 
   datei.close(); // Verbindung zur Datei schließen 
   return 0; 
}
```
### Output:
```
Hello, world!
This is a text file.
Here are some numbers: 1, 2, 3.
```
## Tief eintauchen:
Textdateien existieren schon seit den Anfängen der Computersprachen und werden immer noch häufig verwendet. Eine alternative Möglichkeit, Text zu speichern, ist die Verwendung von Datenbanken. Beim Lesen einer Textdatei muss beachtet werden, dass die Formatierung der einzelnen Zeilen wichtig ist. Die meisten Textdateien verwenden ein bestimmtes Trennzeichen (z.B. ein Komma), um Daten in Spalten zu unterteilen.

## Siehe auch:
- [C++ Referenz - ifstream](http://www.cplusplus.com/reference/fstream/ifstream/)
- [TutorialsPoint - Lesen von Dateien in C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Wikipedia - Textdatei](https://de.wikipedia.org/wiki/Textdatei)