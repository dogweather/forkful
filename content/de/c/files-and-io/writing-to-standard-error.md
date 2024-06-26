---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:49.226331-07:00
description: "Wie geht das: In C wird der `stderr` Stream verwendet, um Fehlermeldungen\
  \ zu schreiben. Anders als bei der Ausgabe auf die Standardausgabe mit `printf`,\u2026"
lastmod: '2024-03-13T22:44:54.370621-06:00'
model: gpt-4-0125-preview
summary: In C wird der `stderr` Stream verwendet, um Fehlermeldungen zu schreiben.
title: Schreiben auf Standardfehler
weight: 25
---

## Wie geht das:
In C wird der `stderr` Stream verwendet, um Fehlermeldungen zu schreiben. Anders als bei der Ausgabe auf die Standardausgabe mit `printf`, kann das Schreiben auf `stderr` mit `fprintf` oder `fputs` erfolgen. So können Sie es tun:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Das ist eine Fehlermeldung.\n");

    fputs("Das ist eine weitere Fehlermeldung.\n", stderr);
    
    return 0;
}
```

Beispielausgabe (auf stderr):
```
Das ist eine Fehlermeldung.
Das ist eine weitere Fehlermeldung.
```

Es ist wichtig zu beachten, dass, obwohl die Ausgabe auf der Konsole ähnlich wie `stdout` erscheint, bei Verwendung der Umleitung im Terminal der Unterschied deutlich wird:

```sh
$ ./your_program > output.txt
```

Dieser Befehl leitet nur die Standardausgabe auf `output.txt` um, während Fehlermeldungen weiterhin auf dem Bildschirm erscheinen.

## Tiefergehend
Die Unterscheidung zwischen `stdout` und `stderr` in Unix-basierten Systemen geht zurück auf die Anfangstage von C und Unix. Diese Trennung ermöglicht eine robustere Fehlerbehandlung und Protokollierung, da sie es Programmierern ermöglicht, Fehlermeldungen unabhängig von der Standardprogrammausgabe umzuleiten. Während `stderr` standardmäßig nicht gepuffert ist, um eine sofortige Ausgabe von Fehlermeldungen zu gewährleisten, was beim Debuggen von Abstürzen und anderen kritischen Problemen hilft, ist `stdout` in der Regel gepuffert, was bedeutet, dass seine Ausgabe verzögert sein könnte, bis der Puffer geleert wird (z. B. bei Programmende oder manuellem Leeren).

In modernen Anwendungen ist das Schreiben auf `stderr` immer noch relevant, besonders bei Kommandozeilen-Tools und Server-Anwendungen, wo die Unterscheidung zwischen regulären Protokollnachrichten und Fehlern entscheidend ist. Jedoch, für komplexere Fehlerbehandlungen, besonders bei GUI-Anwendungen oder wo ausgefeiltere Protokollierungsmechanismen benötigt werden, könnten Programmierer dedizierte Protokollierungsbibliotheken verwenden, die mehr Kontrolle über Nachrichtenformatierung, Ziele (z. B. Dateien, Netzwerk) und Schweregradniveaus (Info, Warnung, Fehler usw.) bieten.

Während `stderr` einen grundlegenden Mechanismus für die Fehlerberichterstattung in C bietet, bedeutet die Entwicklung von Programmierpraktiken und die Verfügbarkeit von fortgeschrittenen Protokollierungsframeworks, dass es oft nur der Ausgangspunkt für moderne Fehlerbehandlungsstrategien ist.
