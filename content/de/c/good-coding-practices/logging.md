---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:03.561730-07:00
description: "Das Protokollieren in C beinhaltet die Aufzeichnung des Ablaufs und\
  \ bemerkenswerter Ereignisse eines Programms w\xE4hrend seiner Laufzeit und bietet\
  \ eine\u2026"
lastmod: '2024-02-25T18:49:51.409638-07:00'
model: gpt-4-0125-preview
summary: "Das Protokollieren in C beinhaltet die Aufzeichnung des Ablaufs und bemerkenswerter\
  \ Ereignisse eines Programms w\xE4hrend seiner Laufzeit und bietet eine\u2026"
title: Protokollierung
---

{{< edit_this_page >}}

## Was & Warum?

Das Protokollieren in C beinhaltet die Aufzeichnung des Ablaufs und bemerkenswerter Ereignisse eines Programms während seiner Laufzeit und bietet eine greifbare Überprüfung seines Verhaltens und seiner Leistung. Programmierer nutzen die Protokollierung für Debugging-Zwecke, Überwachung der Softwaregesundheit und Gewährleistung der Systemsicherheit.

## Wie:

In C kann die Protokollierung mit grundlegenden Dateioperationen oder mithilfe anspruchsvollerer Bibliotheken erreicht werden. Für den Einfachheit halber beginnen wir mit der Standard-E/A-Bibliothek. Die folgenden Snippets zeigen grundlegende Implementierungen der Protokollierung.

Um einfache Nachrichten zu protokollieren:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Öffne die Protokolldatei im Anhängemodus
    
    if (logFile == NULL) {
        perror("Fehler beim Öffnen der Protokolldatei.");
        return -1;
    }
    
    fprintf(logFile, "Anwendung gestartet.\n");
    
    // Hier kommt die Logik Ihrer Anwendung
    
    fprintf(logFile, "Anwendung erfolgreich beendet.\n");
    fclose(logFile);
    
    return 0;
}
```

Ausgabe in `application.log`:

```
Anwendung gestartet.
Anwendung erfolgreich beendet.
```

Um detailliertere Protokolle mit Zeitstempeln und Protokollebenen einzubeziehen:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Entferne das Zeilenumbruchzeichen
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Fehler beim Öffnen der Protokolldatei.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Anwendung startet");
    // Hier kommt die Logik Ihrer Anwendung
    logMessage(logFile, "ERROR", "Ein Beispiel für einen Fehler");
    
    fclose(logFile);
    
    return 0;
}
```

Ausgabe in `detailed.log`:

```
[Do Mär 10 14:32:01 2023] INFO - Anwendung startet
[Do Mär 10 14:32:02 2023] ERROR - Ein Beispiel für einen Fehler
```

## Vertiefung

Wie gezeigt, beruht das Protokollieren in C auf einfachen Dateioperationen, was zwar effektiv, aber nicht so leistungsfähig oder flexibel ist wie Protokollierungseinrichtungen in anderen Sprachen, wie dem `logging` Modul in Python oder `Log4j` in Java. Für fortgeschrittenere Protokollierungsfähigkeiten in C wenden sich Entwickler oft an Bibliotheken wie `syslog` auf Unix-ähnlichen Systemen, die systemweite Protokollverwaltung bieten, oder an Drittanbieterbibliotheken wie `log4c`.

Historisch gesehen war die Protokollierung ein integraler Bestandteil der Programmierung, der auf frühe Programmierpraktiken zurückgeht, bei denen das Nachverfolgen und Verstehen des Programmablaufs und von Fehlern hauptsächlich durch physische Ausdrucke erfolgte. Da sich Systeme weiterentwickelten, wurde die Protokollierung anspruchsvoller und unterstützt jetzt verschiedene Schweregrade von Protokollen, Protokollrotation und asynchrone Protokollierung.

Obwohl die Standardbibliothek von C die grundlegenden Werkzeuge für die Implementierung der Protokollierung bereitstellt, führen ihre Einschränkungen oft zur Schaffung von benutzerdefinierten Protokollierungsframeworks oder zur Annahme externer Bibliotheken für reichhaltigere und flexiblere Protokollierungslösungen. Trotz dieser Einschränkungen ist das Verständnis und die Implementierung der grundlegenden Protokollierung in C entscheidend für das Debugging und die Wartung von Software, insbesondere in Umgebungen, in denen externe Abhängigkeiten minimiert werden sollen.
