---
title:    "C++: Erstellen einer temporären Datei"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum jemand temporäre Dateien erstellen möchte. Eine der Hauptgründe ist, um Daten zwischen verschiedenen Prozessen oder Programmen auszutauschen. Temporäre Dateien können auch verwendet werden, um Zwischenergebnisse zu speichern oder um bestimmte Funktionen in einem Programm zu implementieren, die eine physische Datei erfordern.

## So geht's
Die Erstellung einer temporären Datei in C++ erfordert nur wenige Zeilen Code. Zunächst müssen wir die Header-Datei `fstream` einbinden, um Zugriff auf Dateien zu haben. Dann können wir die Funktion `tmpnam()` verwenden, um einen temporären Dateinamen zu generieren. Anschließend können wir diesen Namen verwenden, um eine Datei mit der Funktion `fopen()` zu erstellen. Hier ist ein Beispiel:

```C++
#include <iostream>
#include <fstream>

int main() {
   char* temp_name = tmpnam(NULL);
   FILE* temp_file = fopen(temp_name, "w+");
   
   if(temp_file == NULL) {
     std::cout << "Fehler beim Erstellen der temporären Datei!" << std::end;
   } else {
     std::cout << "Temporäre Datei erfolgreich erstellt: " << temp_name << std::endl;
     // Weitere Operationen auf der temporären Datei können hier durchgeführt werden
   }
   return 0;
}
```

Die Funktion `tmpnam()` generiert einen eindeutigen Dateinamen und gibt diesen als Zeichenkette zurück. Das `NULL`-Argument gibt an, dass wir keinen zusätzlichen Präfix für den Dateinamen haben möchten. Die Funktion `fopen()` erstellt dann eine Datei mit dem generierten Namen und dem angegebenen Modus. In diesem Beispiel wird die Datei im Lese- und Schreibmodus geöffnet (`w+`).

## Tiefere Einblicke
Die Funktion `tmpnam()` ist Teil der C-Bibliothek und wurde in der neuen Version von C++ (C++11) durch die Funktion `tmpfile()` ersetzt. Diese Funktion erstellt nicht nur einen eindeutigen Dateinamen, sondern auch einen Dateizeiger (FILE object), der verwendet werden kann, um direkt auf die temporäre Datei zuzugreifen.

Außerdem gibt es mit der Funktion `mkstemp()` eine weitere Möglichkeit, um temporäre Dateien zu erstellen. Diese Funktion erstellt eine temporäre Datei und gibt ihren Dateideskriptor als Ganzzahl zurück, was eine einfachere Alternative zur Verwendung von `fopen()` sein kann.

Es ist auch wichtig zu beachten, dass temporäre Dateien automatisch gelöscht werden, wenn das Programm beendet wird oder wenn die Funktion `fclose()` aufgerufen wird.

## Siehe auch
- [C++ Referenz für die Funktion `tmpnam()` (auf Englisch)](https://www.cplusplus.com/reference/cstdio/tmpnam/)
- [C++ Referenz für die Funktion `tmpfile()` (auf Englisch)](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [C++ Referenz für die Funktion `mkstemp()` (auf Englisch)](https://www.cplusplus.com/reference/cstdio/mkstemp/)