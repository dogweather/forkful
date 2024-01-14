---
title:                "C++: Lesen von Befehlszeilenargumente"
simple_title:         "Lesen von Befehlszeilenargumente"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Als Entwickler ist es oft notwendig, Benutzereingaben über die Kommandozeile zu lesen und zu verarbeiten. Dies kann zum Beispiel bei der Erstellung von Skripten oder der Entwicklung von Anwendungen nützlich sein. In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man Befehlszeilenargumente mit C++ einliest und verarbeitet.

## So geht's

Um Befehlszeilenargumente in C++ zu lesen, können wir uns folgender Methode bedienen:

```C++
#include <iostream>
using namespace std;

int main(int argc, char* argv[])
{
    // argc enthält die Anzahl der Argumente
    // argv enthält ein Array von Argumenten-Strings

    // Beispiel: Ausgabe der Befehlszeilenargumente
    for(int i = 0; i < argc; i++)
    {
        cout << argv[i] << endl;
    }

    return 0;
}
```

Dieses Beispiel zeigt wie wir die Argumente in der Kommandozeile auslesen und ausgeben können. Dabei muss beachtet werden, dass das erste Argument (Index 0) immer der Name des Programms selbst ist. Die restlichen Argumente können je nach Bedarf genutzt werden.

## Tiefer Einblick

Betrachten wir nun genauer, was in unserem Beispielcode passiert. Die Funktion `main()` ist der Einstiegspunkt unseres Programms. Sie erwartet als Parameter `argc`, welches die Anzahl der Argumente enthält, und `argv`, welches ein Array von Argumenten-Strings ist. In unserem Beispiel nutzen wir dann eine for-Schleife, um durch dieses Array zu iterieren und die einzelnen Argumente auszugeben.

Es gibt viele weitere Möglichkeiten, Befehlszeilenargumente zu verarbeiten. Zum Beispiel könnten wir bestimmte Argumente auswerten und je nach Eingabe verschiedene Aktionen ausführen. Hier sind der Kreativität keine Grenzen gesetzt.

## Siehe auch

- [C++ Dokumentation: Befehlszeilenargumente lesen](https://www.cplusplus.com/articles/DEN36Up4/)
- [GeeksforGeeks: Befehlszeilenargumente in C++ lesen](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)