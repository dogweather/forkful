---
title:                "Fehlerbehandlung"
date:                  2024-01-26T00:37:08.737011-07:00
model:                 gpt-4-1106-preview
simple_title:         "Fehlerbehandlung"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/handling-errors.md"
---

{{< edit_this_page >}}

## Was & Warum?
Fehlerbehandlung in C bedeutet, mit dem Unerwarteten zu rechnen. Sie verhindert, dass Programme verrückt spielen, wenn sie auf Probleme stoßen. Entwickler tun dies, um Fehler elegant zu behandeln und ihren Code zuverlässig zu gestalten.

## Wie geht das:

Sehen wir uns an, wie das in C geht:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nichtvorhandenedatei.txt", "r");
    if (fp == NULL) {
        perror("Fehler beim Öffnen der Datei");
        return EXIT_FAILURE;
    }
    // Etwas mit der Datei machen
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Beispiel-Ausgabe, wenn die Datei nicht existiert:
```
Fehler beim Öffnen der Datei: Datei oder Verzeichnis nicht gefunden
```

## Vertiefung

In den Anfangstagen von C war die Fehlerbehandlung rudimentär - meist Rückgabecodes und manuelle Überprüfungen. Dann kam `errno`, eine globale Variable, die aktualisiert wird, wenn Funktionen scheitern. Sie ist an sich nicht thread-sicher, daher wurden die neueren Funktionen `strerror` und `perror` für eine bessere Fehlerberichterstattung eingeführt.

Alternativen? Modernes C ist nicht auf `errno` beschränkt. Es gibt setjmp und longjmp für nicht-lokale Sprünge, wenn eine Katastrophe eintritt. Einige bevorzugen die Definition eigener Fehlercodes, während andere sich für Ausnahme-ähnliche Strukturen in C++ entscheiden.

Die Implementierungsdetails können komplex sein. Zum Beispiel ist `errno` in POSIX-konformen Systemen threadsicher, dank der Magie des Thread-Local Storage (TLS). In eingebetteten Systemen, wo Ressourcen kostbar sind, könnte benutzerspezifischer Fehlerbehandlungscode gegenüber Standardansätzen bevorzugt werden, die die Software aufblähen könnten.

## Siehe auch

- Eine detaillierte Einführung in `errno`: https://de.cppreference.com/w/c/error/errno
- Für Thread-Sicherheit siehe POSIX-Threads und errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Eine Einführung in setjmp und longjmp: https://de.cplusplus.com/reference/csetjmp/
- Für Ausnahmebehandlung in C++ siehe: https://isocpp.org/wiki/faq/exceptions