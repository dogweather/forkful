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

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist eine häufige Aufgabe in der Programmierung. Dabei werden die Argumente, die bei der Ausführung des Programms in der Befehlszeile angegeben wurden, vom Programm eingelesen und verwendet. Dies ermöglicht es dem Programmierer, die Ausführung seines Programms zu steuern und anzupassen.

## Wie geht's?

Um Befehlszeilenargumente in C zu lesen, können wir die vordefinierten Variablen `argc` und `argv` verwenden. `argc` enthält die Anzahl der Argumente, die in der Befehlszeile angegeben wurden, und `argv` ist ein Array von Zeigern auf die einzelnen Argumente. Wir können dann einfach über `argv` iterieren und auf die Argumente zugreifen. Zum Beispiel:

```
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("Anzahl der Argumente: %d \n", argc);
  for (int i=0; i<argc; i++) {
    printf("Argument %d: %s \n", i, argv[i]);
  }
  return 0;
}
```
Wenn wir dieses Programm als `./programmname argument1 argument2` ausführen, erhalten wir die Ausgabe:

```
Anzahl der Argumente: 3
Argument 0: ./programmname
Argument 1: argument1
Argument 2: argument2
```

## Tiefere Einblicke

Das Lesen von Befehlszeilenargumenten ist eine gängige Aufgabe in vielen Programmiersprachen. Neben C unterstützen auch Sprachen wie Python, Java und Perl das Lesen von Befehlszeilenargumenten auf ähnliche Weise.

Eine alternative Möglichkeit besteht darin, eine externe Bibliothek zu verwenden, die das Parsen von Befehlszeilenargumenten erleichtert, wie zum Beispiel `getopt` oder `argp` in C.

Bei der Implementierung von Befehlszeilenargumenten in C ist es wichtig, auf die Reihenfolge der Argumente und die Behandlung von Optionen und Argumenten zu achten. Es kann auch hilfreich sein, Fehlerbehandlungen und Validierungen hinzuzufügen, um die Benutzerfreundlichkeit des Programms zu verbessern.

## Siehe auch

- [C-Programmierhandbuch: Befehlszeilenargumente](https://www.gnu.org/software/libc/manual/html_node/Command_002dLine-Arguments.html)
- [How to Read Command Line Arguments in C](https://www.geeksforgeeks.org/how-to-read-command-line-arguments-in-c/)
- [Using Command Line Arguments in C](https://www.codingunit.com/using-command-line-arguments-in-c)