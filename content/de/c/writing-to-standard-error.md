---
title:                "C: Schreiben zum Standardfehler"
simple_title:         "Schreiben zum Standardfehler"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum?

Das Schreiben in die Standardfehlerausgabe ist eine häufig verwendete Technik in der C-Programmierung. Es ermöglicht dem Programmierer, Fehlermeldungen und andere relevante Informationen anzuzeigen, ohne den Hauptfokus des Programms zu unterbrechen.

# Wie funktioniert das?

Mit der Funktion `fprintf()` kann eine Ausgabe in die Standardfehlerausgabe geschrieben werden. Diese Funktion ähnelt der bekannten `printf()` Funktion, verwendet jedoch den Standardfehlerstrom `stderr` anstelle des Standardausgabe-Stroms `stdout`.

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "Dies ist ein Beispiel für die Verwendung von fprintf() in der Standardfehlerausgabe.\n");
  return 0;
}
```

`stderr` wird in der Regel auf dem Terminal ausgegeben, so dass der Benutzer die Fehlermeldungen und Informationen sofort sehen kann.

```C
Dies ist ein Beispiel für die Verwendung von fprintf() in der Standardfehlerausgabe.
```

# Tiefer in die Materie eintauchen

Das Schreiben in die Standardfehlerausgabe ist besonders nützlich bei der Fehlersuche und dem Debuggen von Programmen. Indem man wichtige Informationen und Fehlermeldungen in die Standardfehlerausgabe schreibt, kann man das Programm verfolgen und mögliche Probleme schnell erkennen.

Man kann auch die Funktion `perror()` verwenden, um eine falsche Rückgabewert auszugeben, indem man einen beschreibenden Fehlerstring ausgibt, der den Wert der `errno`-Variablen verwendet.

```C
#include <stdio.h>
#include <errno.h>

int main() {
  FILE *fp = fopen("file.txt", "r");
  if (fp == NULL) {
    perror("Fehler beim Öffnen der Datei: ");
    return 0;
  }

  return 0;
}
```

Die Ausgabe würde wie folgt aussehen, wenn die Datei nicht gefunden wird:

```C
Fehler beim Öffnen der Datei: No such file or directory
```

# Siehe auch

- [Die offizielle Dokumentation zu fprintf()](https://www.gnu.org/software/libc/manual/html_node/Formatted-Output.html)
- [Einführung in die Standardfehlerausgabe und errno in der C-Programmierung](https://www.geeksforgeeks.org/error-handling-c-programs/)
- [Die offizielle Dokumentation zu perror()](https://www.gnu.org/software/libc/manual/html_node/Error-Messages.html)