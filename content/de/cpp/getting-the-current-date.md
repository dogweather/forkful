---
title:    "C++: Das aktuelle Datum erhalten"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Warum
Es gibt viele Gründe, warum Entwickler das aktuelle Datum in ihrem Programm benötigen können. Zum Beispiel, um ein Log-File zu erstellen, um die Aktualität von Daten zu überprüfen oder um bestimmte Funktionen basierend auf dem Datum auszuführen. Glücklicherweise ist es in C++ relativ einfach, das aktuelle Datum zu bekommen.

# How To
```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // aktuelles Datum und Zeit bekommen
    time_t now = time(0);
    
    // Umwandlung in String-Format
    char* date = ctime(&now);
    
    // Ausgabe des Datums
    cout << "Das aktuelle Datum und die Uhrzeit ist: " << date << endl;
    
    // Umwandlung in struct tm-Format
    tm *ltm = localtime(&now);
    
    // Ausgabe der einzelnen Komponenten des Datums
    cout << "Jahr: " << 1900 + ltm->tm_year << endl;
    cout << "Monat: " << 1 + ltm->tm_mon << endl;
    cout << "Tag: " << ltm->tm_mday << endl;
    cout << "Stunde: " << ltm->tm_hour << endl;
    cout << "Minute: " << ltm->tm_min << endl;
    cout << "Sekunde: " << ltm->tm_sec << endl;
    
    return 0;
}
```
Die Ausgabe sieht wie folgt aus:

```
Das aktuelle Datum und Uhrzeit ist: Fri Mar 12 16:24:07 2021
Jahr: 2021
Monat: 3
Tag: 12
Stunde: 16
Minute: 24
Sekunde: 7
```
# Deep Dive
Um das aktuelle Datum und die Uhrzeit in C++ zu bekommen, verwenden wir die Bibliothek "ctime" und deren Funktion "time". Diese Funktion gibt die Anzahl der Sekunden, die seit dem 1. Januar 1970 vergangen sind, zurück. Wir speichern diese Anzahl in einer Variable vom Typ time_t. 
Dann verwenden wir die Funktion "ctime", um das Datum im String-Format zu bekommen. 
Um Zugriff auf die einzelnen Komponenten des Datums zu haben, wandeln wir es in das struct tm-Format um und speichern es in einer Variable vom Typ tm. 
Von dort aus können wir die einzelnen Komponenten wie Jahr, Monat und Tag mit Hilfe von "tm_year", "tm_mon" und "tm_mday" ausgeben.

# Siehe Auch
- [Die offizielle Dokumentation von ctime](https://www.cplusplus.com/reference/ctime/)
- [Beispielprogramme zum Thema Datums- und Zeitfunktionen in C++](https://www.geeksforgeeks.org/time-functions-in-c-c/)
- [Wie man das Datum in verschiedenen Formaten in C++ ausgibt](https://www.guru99.com/cpp-date-time.html)