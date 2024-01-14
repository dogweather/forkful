---
title:                "C++: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum
CSV-Dateien sind ein beliebtes Format für die Speicherung und den Austausch von Daten. Wenn Sie mit Programmierung arbeiten, werden Sie mit großer Wahrscheinlichkeit auf CSV-Dateien stoßen. Das Verständnis und der Einsatz von CSV-Dateien kann Ihre Arbeit erleichtern und beschleunigen.

## Wie Geht Es
Um mit CSV-Dateien in C++ zu arbeiten, müssen Sie zunächst die <fstream> und <sstream> Bibliotheken einbinden. Dann können Sie eine CSV-Datei öffnen und die Daten in speicherbare Variablen konvertieren.

Eine Beispielcode für das Öffnen und Lesen einer CSV-Datei kann folgendermaßen aussehen:

```C++
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

int main() {

    // CSV-Datei öffnen
    ifstream csvFile("beispiel.csv");

    // Variable für Datenzeile deklarieren
    string datenzeile;

    // Schleife zum Lesen der Datei
    while (getline(csvFile, datenzeile)) {

        // Datenstring in Stream konvertieren
        stringstream stream(datenzeile);

        // Variablen deklarieren und Daten in sie speichern
        string name;
        int alter;
        stream >> name >> alter;

        // Ausgabe der Daten
        cout << "Name: " << name << ", Alter: " << alter << endl;
    }

    // Datei schließen
    csvFile.close();

    return 0;
}
```

Die obige Code Beispiel liest eine CSV-Datei mit dem Inhalt "Max,25" und gibt die Daten "Name: Max, Alter: 25" aus.

## Tiefer Einblick
Die <sstream> Bibliothek ist besonders nützlich, da sie es ermöglicht, CSV-Daten in verschiedene Variablentypen zu konvertieren. Sie können beispielsweise auch boolesche Werte und Fließkommazahlen aus einer CSV-Datei auslesen.

Außerdem ist es wichtig zu beachten, dass CSV-Dateien häufig Sonderzeichen oder unterschiedliche Trennzeichen verwenden. In solchen Fällen müssen Sie möglicherweise eine Funktion zur Zeichenumwandlung verwenden, um die Daten korrekt zu lesen.

## Siehe Auch
- Offizielle C++ Dokumentation für <fstream>: https://en.cppreference.com/w/cpp/header/fstream
- Offizielle C++ Dokumentation für <sstream>: https://en.cppreference.com/w/cpp/header/sstream
- Tutorial für den Umgang mit CSV-Dateien in C++: https://www.binpress.com/working-with-csv-files-in-c/