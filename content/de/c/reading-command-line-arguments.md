---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:55:37.774683-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Kommandozeilenargumente zu lesen bedeutet, dass dein Programm beim Start Werte von außen aufnehmen kann. Diese Flexibilität erlaubt es, das Verhalten des Programms ohne Codeänderungen anzupassen.

## So geht’s:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Argumentanzahl: %d\n", argc);
    for(int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Wird das Programm so ausgeführt: `./deinprogramm Hallo Welt`, ist die Ausgabe:

```
Argumentanzahl: 3
Argument 0: ./deinprogramm
Argument 1: Hallo
Argument 2: Welt
```

## Tiefgang:

Die Übergabe von Kommandozeilenargumenten ist so alt wie C selbst, zurückgehend auf die 1970er Jahre. Ursprünglich für Unix-Systeme entwickelt, ist dieser Ansatz standardmäßig in fast allen Betriebssystemen zu finden.

Es gibt Alternativen – wie Umgebungsvariablen oder Konfigurationsdateien – doch direkte Kommandozeilenargumente bieten die schnellste und einfachste Methode zur Parameterübergabe.

Die `main()`-Funktion akzeptiert hier zwei Parameter: `argc` (argument count) gibt die Anzahl der Argumente an, `argv` (argument vector) ist ein Array aus Zeigern auf die Argumente selbst. Der erste Eintrag in `argv` (`argv[0]`) ist immer der Name bzw. der Pfad des ausgeführten Programms.

## Siehe auch:

- C Standard Library Dokumentation zu `<stdlib.h>` und `<stdio.h>`: https://en.cppreference.com/w/c/header
- GNU Program Documentation on Command Line Arguments: https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html
- "C Programming Language" (2. Ausgabe) von Brian W. Kernighan und Dennis M. Ritchie – eine gründliche Einführung in die Programmierung in C.
