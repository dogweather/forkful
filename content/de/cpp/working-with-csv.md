---
title:                "Arbeiten mit CSV"
html_title:           "C++: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Was ist CSV und Warum solltest du es nutzen?

CSV - kurz für Comma-Separated Values - ist ein Dateiformat, das häufig von Programmierern verwendet wird, um strukturierte Daten zu speichern und zu verarbeiten. Es besteht im Wesentlichen aus Textdaten, die in Form von Tabellen organisiert sind und durch Kommata getrennt werden.

Programmierer nutzen CSV, um effizient große Mengen an Daten zu verarbeiten, da es ein einfaches und flexibles Format ist. Es ermöglicht auch die einfache Weitergabe von Daten zwischen verschiedenen Programmen und Plattformen. 

## Wie funktioniert es?

Um mit CSV in C++ zu arbeiten, musst du zunächst die "fstream" Bibliothek einbinden. Dann kannst du die Datei mit Hilfe der "ifstream" Klasse öffnen, die dann die Daten in einem Array abspeichert. Du kannst dann mit Schleifen oder verschiedenen Methoden auf die Daten zugreifen und sie bearbeiten.

Ein Beispiel:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    ifstream file("daten.csv"); // Öffne die Datei "daten.csv"
    string array[3]; // Erstelle ein Array mit 3 Elementen (Zahlen, Text, Text)
    
    for (int i=0; i<3; i++) // Durchlaufe das Array
    {
        getline(file, array[i], ','); // Lese die Daten aus der Datei und trenne sie bei jedem Komma
    }
    
    for (int i=0; i<3; i++) // Durchlaufe das Array erneut
    {
        cout << array[i] << endl; // Gebe die Daten aus
    }
    
    file.close(); // Schließe die Datei
    
    return 0;
}
```

Output:

```
123
Text1
Text2
```

## Tiefere Einblicke

CSV wurde in den 1970er Jahren entwickelt und war lange Zeit das Standardformat für den Austausch von Daten zwischen Tabellenkalkulationsprogrammen. Es ist einfacher als XML oder JSON und wird daher oft für kleinere und weniger komplexe Datensätze verwendet. 

Es gibt auch alternative Formate wie TSV (Tab-Separated Values) oder Delimited-ASCII, die ähnlich funktionieren wie CSV, aber mit anderen Trennzeichen arbeiten.

In der C++ Implementierung gibt es verschiedene Methoden, um mit CSV-Daten umzugehen, z.B. das Auslesen von bestimmten Spalten oder das Hinzufügen von neuen Zeilen. Es ist auch möglich, CSV-Daten in strukturiertere Formate, wie z.B. ein Array von benutzerdefinierten Objekten, zu konvertieren.

## Weitere Informationen

- [CSV auf Wikipedia](https://de.wikipedia.org/wiki/CSV_(Dateiformat))
- [C++ CSV Parser Library](https://github.com/ben-strasser/fast-cpp-csv-parser)
- [CSV vs. XML vs. JSON](https://www.alfresco.com/blogs/developer/2013/05/28/data-interchange-csv-json-xml/)