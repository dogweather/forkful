---
title:    "C: Lesen von Befehlszeilenargumenten"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum
Das Lesen von Befehlszeilenargumenten ist eine grundlegende Fähigkeit in der Programmierung. Es ermöglicht dem Benutzer, Eingaben direkt an das Programm zu übergeben, was es flexibler und benutzerfreundlicher macht.

## Wie man Befehlszeilenargumente liest
Befehlszeilenargumente werden verwendet, um Eingaben an ein Programm zu übergeben, die es dann verarbeiten kann. In der Sprache C können Befehlszeilenargumente leicht mit der ```main()``` Funktion gelesen werden.

Hier ist ein Beispielcode, der alle Befehlszeilenargumente auf dem Bildschirm ausgibt:

```C
#include <stdio.h>

int main(int argc, char *argv[])
{
    // Schleife, um alle Argumente auszugeben
    for(int i = 0; i < argc; i++)
    {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    
    return 0;
}
```

Wenn wir diesen Code mit folgenden Befehlszeilenargumenten kompilieren und ausführen:

```
./programm arg1 arg2 arg3
```

würde die Ausgabe wie folgt aussehen:

```
Argument 0: ./programm
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

Wir können auch auf spezifische Argumente zugreifen, indem wir ihre Indexposition im ```argv``` Array verwenden. Zum Beispiel könnte man einen Integer-Wert als Argument übergeben und ihn in eine Variable konvertieren:

```C
int num = atoi(argv[1]); // Argument 1 in Variable num speichern
```

## Tiefere Einblicke
In der Regel sind Befehlszeilenargumente optional und müssen nicht vom Benutzer eingegeben werden. Wenn jedoch erwartet wird, dass bestimmte Argumente übergeben werden, können entsprechende Überprüfungen in der Code-Logik eingebaut werden.

In komplexeren Programmen können auch Optionen und Flags als Befehlszeilenargumente verwendet werden, um das Verhalten des Programms anzupassen. Dazu müssen die Argumente analysiert und entsprechend verarbeitet werden.

Zusätzlich zur Verarbeitung von Eingaben von Benutzern können Befehlszeilenargumente auch beim Debuggen von Programmen hilfreich sein, indem sie direkt an das Programm übergeben werden, anstatt sie jedes Mal manuell einzugeben.

## Siehe auch
- [Befehlszeilenargumente in C - Tutorialspoint auf Deutsch](https://www.tutorialspoint.com/de/cprogramming/c_command_line_arguments.htm)
- [Das allgemeine Format von Befehlszeilenargumenten - cppreference auf Deutsch](https://de.cppreference.com/w/cpp/language/main_function)
- [Befehlszeilenargumente in C - GeeksforGeeks auf Deutsch](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)