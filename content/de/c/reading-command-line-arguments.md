---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "C: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wer in der Programmierung mit C arbeitet, wird nicht umhin kommen, auch mit Kommandozeilen-Argumenten zu tun zu haben. Diese erlauben es, dem Programm bei jedem Aufruf unterschiedliche Werte zu übergeben. In diesem Artikel erfahrt ihr, wie ihr in C auf diese Argumente zugreift und sie im Programm nutzen könnt.

## So funktioniert's

Um die Argumente aus der Kommandozeile auszulesen, wird die main-Funktion verwendet. Deren Argumente sind ein Integer, der die Anzahl der Argumente enthält, sowie ein Array von Strings mit den einzelnen Argumenten.

Ein Beispiel-Programm könnte so aussehen:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    // Argumente ausgeben
    printf("Anzahl der Argumente: %d\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Wenn dieses Programm mit den Argumenten "Hallo Welt" aufgerufen wird, würde die Ausgabe folgendermaßen aussehen:

```
Anzahl der Argumente: 3
Argument 0: ./programm
Argument 1: Hallo
Argument 2: Welt
```

Bei der Ausführung eines Programms werden auch immer mindestens ein Argument übergeben, nämlich der Name des Programms selbst. Daher wird das erste Argument als "./programm" angezeigt.

## Tiefergehende Informationen

Die Argumente können nicht nur ausgelesen, sondern auch zur Verarbeitung im Programm genutzt werden. Dabei ist zu beachten, dass die Argumente immer als Strings übergeben werden und bei Bedarf noch in den jeweiligen Datentyp umgewandelt werden müssen.

Ein weiteres wichtiges Konzept sind Optionen. Diese werden häufig bei Aufrufen von Befehlszeilenprogrammen verwendet, um bestimmte Verhaltensweisen festzulegen. Sie werden durch einen Bindestrich und einen Buchstaben angegeben, z.B. "-v" für die Ausgabe von Versioninformationen. Um diese Optionen zu verarbeiten, können Funktionen wie "strcmp" und "getopt" aus der Standardbibliothek verwendet werden.

## Siehe auch

- Dokumentation des C Standard Library: https://en.cppreference.com/w/c
- Einsteiger-Tutorial zur Programmierung mit C: https://www.learn-c.org/
- Mehr über Befehlszeilenargumente in C: https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm