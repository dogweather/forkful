---
title:                "Protokollierung"
date:                  2024-01-26T00:59:56.832799-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?
Logging bedeutet im Wesentlichen, festzuhalten, was Ihr Programm tut, typischerweise indem Nachrichten in eine Datei oder auf das Terminal geschrieben werden. Programmierer tun dies, um Ereignisse zu verfolgen, Probleme zu diagnostizieren und um eine Prüfspur zu haben, die die Geschichte des Betriebs einer Anwendung über die Zeit erzählt.

## Wie geht das:
Beginnen wir mit einigen Grundlagen. C verfügt nicht über ein integriertes Logging-Framework, aber Sie können etwas Einfaches mit `stdio.h` zusammenstellen. So geht das:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *date = ctime(&now);
    date[strlen(date) - 1] = '\0'; // Entfernt den Zeilenumbruch am Ende des Ergebnisses von ctime()
    printf("[%s] %s\n", date, message);
}

int main() {
    logMessage("Anwendung gestartet.");
    // ... Ihr Code folgt hier ...
    logMessage("Anwendung führt etwas Wichtiges aus.");
    // ... Ihr Code geht weiter ...
    logMessage("Anwendung beendet.");
    return 0;
}
```

Eine Beispiel-Ausgabe könnte so aussehen:

```
[Di Mär 9 12:00:01 2023] Anwendung gestartet.
[Di Mär 9 12:00:02 2023] Anwendung führt etwas Wichtiges aus.
[Di Mär 9 12:00:03 2023] Anwendung beendet.
```

Natürlich würden Sie in der realen Welt wahrscheinlich eher in eine Datei statt in das Terminal schreiben wollen, unterschiedliche Log-Level behandeln und vielleicht eine vordefinierte Bibliothek verwenden.

## Vertiefung
Das Logging in C hat einen gewissen Charme – es ist so low-level wie der Großteil der Sprache. Historisch gesehen wurde das Logging mit `fprintf` und `stderr` oder einem Dateizeiger durchgeführt. Mit zunehmender Komplexität der Programme wurden auch die Anforderungen an das Logging komplexer, was zur Entwicklung von Bibliotheken wie `syslog` auf Unix-Systemen führte, die das Logging von mehreren Quellen mit verschiedenen Wichtigkeitsstufen handhaben konnten.

In der modernen Landschaft gibt es viele C-Logging-Bibliotheken wie `zlog`, `log4c` und `glog`, die einen reichhaltigen Funktionsumfang bieten, einschließlich Log-Rotation, strukturiertem Logging und multithreaded Logging. Diese Lösungen ermöglichen eine feingranulare Kontrolle über Log-Verbosity, Ziele und Formate.

Beim Implementieren eines Logging-Systems müssen Details wie das Formatieren von Zeitstempeln, das Management von Log-Dateien und die Performance berücksichtigt werden. Das Zeitstempeln von Logs ist entscheidend für das Korrelieren von Ereignissen, während Log-Rotation sicherstellt, dass Log-Dateien nicht zu viel Speicherplatz verbrauchen. Der Akt des Loggings sollte auch schnell und nicht-blockierend für den Hauptanwendungsfluss sein, um zu verhindern, dass das Logging zu einem Flaschenhals wird.

## Siehe auch
Um tiefer in Logging-Bibliotheken und -Praktiken in C einzusteigen, schauen Sie sich diese Ressourcen an:

- GNU `syslog` Handbuch: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Eine hochkonfigurierbare Logging-Bibliothek für C - https://github.com/HardySimpson/zlog
- `log4c`: Ein nach Log4j modelliertes Logging-Framework für C - http://log4c.sourceforge.net/
- `glog`: Googles anwendungsspezifische Logging-Bibliothek - https://github.com/google/glog
