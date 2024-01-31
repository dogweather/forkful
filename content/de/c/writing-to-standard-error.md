---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
simple_title:         "Schreiben auf Standardfehler"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben auf Standardfehler (stderr) bedeutet, Fehlermeldungen und Diagnosedaten separat von normalen Ausgaben (stdout) zu behandeln. Entwickler nutzen dies, um Probleme transparent zu kommunizieren, ohne die Standardausgabe zu stören.

## How to:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Fehler: Eine kritische Ausnahme ist aufgetreten!\n");
    return 1; // Ein Programmstatus ungleich 0 signalisiert einen Fehler.
}
```

Ausgabe:
```
Fehler: Eine kritische Ausnahme ist aufgetreten!
```

## Deep Dive
Das Schreiben auf stderr hat seinen Ursprung in Unix-Systemen. Es ermöglicht die Trennung von Standardausgabe und Fehlerausgabe, was das Debugging erleichtert. Alternativen, wie das Schreiben in eine Log-Datei, werden situationsabhängig genutzt. In der Implementierung ist stderr ein File-Stream mit ungepuffertem Output, was bedeutet, dass Nachrichten sofort ausgegeben werden.

## See Also
- C Standard Library Documentation on stderr: https://en.cppreference.com/w/c/io/std_streams
- Linux Programmer's Manual on Standard Streams: https://man7.org/linux/man-pages/man3/stdin.3.html
- Effective C: An Introduction to Professional C Programming, Robert C. Seacord
