---
title:    "C++: Eine Textdatei lesen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit in der Programmierung und kann dabei helfen, Daten effektiv zu verarbeiten und auszugeben. Obwohl es eine grundlegende Aufgabe ist, ist es wichtig, zu wissen, wie man Textdateien in einer Programmiersprache wie C++ richtig liest, da dies in vielen Anwendungsfällen nützlich sein kann.

## Wie geht das?

Um eine Textdatei in C++ zu lesen, müssen Sie einige grundlegende Schritte befolgen:

```C++
#include <iostream>
#include <fstream>

int main()
{
    // Textdatei erstellen und öffnen
    std::ofstream file("meine_datei.txt");

    // Überprüfen, ob die Datei erfolgreich geöffnet wurde
    if (file.is_open()) 
    {
        // Text in die Datei schreiben
        file << "Hallo Welt!" << endl;
        file << "Dies ist eine Textdatei zum Lesen." << endl;

        // Datei schließen
        file.close();
    }
    else 
    {
        // Fehlermeldung, falls die Datei nicht geöffnet werden konnte
        std::cout << "Fehler beim Öffnen der Datei." << endl;
    }

    // Textdatei zum Lesen öffnen
    std::ifstream readfile("meine_datei.txt");

    // Überprüfen, ob die Lese-Datei erfolgreich geöffnet wurde
    if (readfile.is_open()) 
    {
        // Zeile für Zeile aus der Datei lesen und ausgeben
        std::string line;
        while (std::getline(readfile, line)) 
        {
            std::cout << line << endl;
        }

        // Datei schließen
        readfile.close();
    }
    else 
    {
        // Fehlermeldung, falls die Datei nicht geöffnet werden konnte
        std::cout << "Fehler beim Öffnen der Datei." << endl;
    }

    return 0;
}
```

Die obigen Beispielcodes erstellen eine Textdatei mit dem Namen "meine_datei.txt", schreiben einige Textzeilen hinein und lesen sie dann wieder aus. Indem wir die Funktionen `ofstream` und `ifstream` aus der Bibliothek `fstream` verwenden, können wir Dateien erstellen und lesen sowie verschiedene Dateioperationen durchführen.

## Tieferer Einblick

Bei der Verwendung von Dateistreams zum Lesen von Textdateien in C++ sind einige Dinge zu beachten. Grundlegend ist es wichtig, die Datei zu öffnen und zu überprüfen, ob sie erfolgreich geöffnet wurde, bevor wir versuchen, in sie zu schreiben oder aus ihr zu lesen. Außerdem müssen wir uns bewusst sein, welche Datenstrukturen wir verwenden, um den Text zu lesen, z.B. die `string`-Klasse oder `char`-Arrays.

Ein weiterer wichtiger Aspekt ist die Behandlung von Dateipfaden. In unserem Beispiel haben wir lediglich den Dateinamen angegeben, da unsere Programmdatei und die Textdatei im selben Ordner liegen. Um jedoch auf Dateien in anderen Ordnern zuzugreifen, müssen wir den vollständigen Pfad zur Datei angeben oder die Datei im Ordner unseres Programms platzieren.

Es ist auch wichtig zu beachten, dass beim Lesen von Textdateien möglicherweise Formatierungszeichen wie Zeilenumbrüche enthalten sind, die berücksichtigt werden müssen. Dies kann zu Problemen führen, wenn wir versuchen, diese Zeichen zu ignorieren oder zu überschreiben.

Insgesamt ist das Lesen von Textdateien in C++ ein grundlegender, aber wichtiger Prozess, der uns hilft, effektiv mit Daten zu arbeiten und sie zu manipulieren.

## Siehe auch

- [C++ Dokumentation zur Bibliothek fstream](https://www.cplusplus.com/reference/fstream/)

- [Tutorial: Lesen und Schreiben von Textdateien in C++](https://www.tutorialspoint.com/cplusplus/reading_from_file.htm)

- [Beispielprogramme für das Lesen von Textdateien in C++](https://www.programiz.com/cpp-programming/examples/read-file)