---
title:                "C: Lesen von Befehlszeilenargumenten"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Warum

Beim Programmieren in C können wir nicht immer vorhersehen, welche Eingaben der Benutzer machen wird. Hier kommen Befehlszeilenargumente ins Spiel. Sie ermöglichen es uns, Eingaben zur Laufzeit des Programms zu erhalten und zu verarbeiten. In diesem Blogpost werden wir uns ansehen, wie man Befehlszeilenargumente in C liest.

##So geht's

Zunächst müssen wir in unserem Code die main-Funktion definieren. Diese wird das Hauptschleifenelement unseres Programms sein. Innerhalb der main-Funktion können wir dann unsere Befehlszeilenargumente lesen und verarbeiten.

```C
int main(int argc, char *argv[]) {

    // Code zum Lesen und Verarbeiten der Befehlszeilenargumente

} 
```

Als nächstes müssen wir überprüfen, ob überhaupt Argumente übergeben wurden. Dazu nutzen wir die Variabel `argc`, die die Anzahl der Argumente speichert. Im Beispielcode können wir sehen, dass sie als erster Parameter in der main-Funktion definiert wird. In der Regel wird sie mit dem Wert 1 initialisiert, da der erste Parameter immer der Programmname ist. Wenn also keine weiteren Argumente übergeben werden, ist `argc` gleich 1.

```C
if (argc == 1) {
    // Keine Argumente übergeben
}
```

Wenn jedoch Argumente übergeben wurden, können wir sie mit der `argv` Variabel auslesen. Diese speichert alle übergebenen Argumente als Strings in einem Array. Der erste Index, `argv[0]`, beinhaltet dabei immer den Programmnamen.

Um alle Argumente auszulesen, können wir eine Schleife nutzen, die über das `argv` Array läuft. Ein Beispiel dafür wäre:

```C
for (int i = 1; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
}
```

Im obigen Beispiel wird jeder Index aus `argv` ausgelesen und gemeinsam mit dem entsprechenden Index als Ausgabe angezeigt. Das bedeutet, dass `argv[1]` der erste übergebene Parameter, `argv[2]` der zweite und so weiter ist. Wenn also zum Beispiel folgendes Kommando ausgeführt wird: `./programmname parameter1 parameter2`, dann wäre die Ausgabe des oben gezeigten Codes:

```
Argument 1: parameter1
Argument 2: parameter2
```

##Tiefer eintauchen

Neben den Standardfunktionen `argc` und `argv` gibt es noch eine weitere Möglichkeit, Befehlszeilenargumente in C auszulesen: `getopt`. Diese Funktion ermöglicht es, Optionen und Argumente in einem bestimmten Format auszulesen. Dazu müssen wir jedoch die Headerdatei `unistd.h` einbinden und etwas mehr Code schreiben.

Eine Beispielcode für `getopt` könnte so aussehen:

```C
#include <unistd.h>

int main(int argc, char *argv[]) {

    int option;

    // Optionen definieren
    while ((option = getopt(argc, argv, "d::f:")) != -1) {
        switch (option) {
            case 'd':
                // Code für Option -d
            case 'f':
                // Code für Option -f
            case '?':
                // Code für ungültige Option
        }
    }

}
```

In diesem Beispiel definieren wir zwei Optionen: `-d` und `-f`. Die erste Option `-d` beinhaltet keine Argumente, während die Option `-f` ein Argument erwartet. Mit `getopt` können wir dann überprüfen, ob eine dieser Optionen bei der Programmausführung mit angegeben wurde. Abhängig von der angegebenen Option kann dann entsprechender Code ausgeführt werden.

Es gibt noch viele weitere Möglichkeiten, Befehlszeilenargumente in C zu lesen und zu verarbeiten. Eine tiefergehende Auseinandersetzung mit Optionen und ihren Argumenten kann sehr nützlich sein, um komplexe Befehlszeilenprogramme zu erstellen.

##Siehe auch

- [GNU Libc - Command Line Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments)
- [Tutorialspoint - C - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [GeeksforGeeks - Command line arguments in C