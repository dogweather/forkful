---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:18.781310-07:00
description: "Das Erstellen einer tempor\xE4ren Datei in C bedeutet, eine Datei zu\
  \ generieren, die f\xFCr eine kurze Dauer gedacht ist, \xFCblicherweise als Zwischenspeicher\
  \ f\xFCr\u2026"
lastmod: '2024-03-13T22:44:54.374262-06:00'
model: gpt-4-0125-preview
summary: "Das Erstellen einer tempor\xE4ren Datei in C bedeutet, eine Datei zu generieren,\
  \ die f\xFCr eine kurze Dauer gedacht ist, \xFCblicherweise als Zwischenspeicher\
  \ f\xFCr Datenverarbeitung oder -speicherung."
title: "Erstellung einer tempor\xE4ren Datei"
weight: 21
---

## Wie:
Das Erstellen einer temporären Datei in der Programmiersprache C kann Funktionen wie `tmpfile()` und `mkstemp()` nutzen.

**Verwendung von `tmpfile()`**: Diese Funktion erstellt eine einzigartige temporäre Datei, die automatisch gelöscht wird, wenn das Programm beendet wird oder die Datei geschlossen wird.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Fehler beim Erstellen der temporären Datei");
        return 1;
    }

    // Schreiben von Daten in die temporäre Datei
    fputs("Dies ist ein Test.\n", temp);

    // Zurückspulen und lesen, was wir geschrieben haben
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Wird automatisch beim Schließen oder Programmende gelöscht
    fclose(temp);

    return 0;
}
```
**Beispielausgabe:**
```
Dies ist ein Test.
```

**Verwendung von `mkstemp()`**: Bietet mehr Kontrolle über den Speicherort der temporären Datei und deren Berechtigungen. Es erfordert eine Vorlagenzeichenkette, die mit `XXXXXX` endet, welche dann mit einer einzigartigen Sequenz ersetzt wird, um Namenskonflikte zu vermeiden.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Fehler beim Erstellen der temporären Datei");
        return 1;
    }
    
    printf("Temporäre Datei erstellt: %s\n", template);

    // Temporäre Dateien, die mit mkstemp() erstellt wurden, sollten manuell gelöscht werden
    unlink(template);

    close(fd);
    return 0;
}
```
**Beispielausgabe:**
```
Temporäre Datei erstellt: /tmp/mytemp-abc123
```

## Vertiefung
Das Konzept von temporären Dateien ist nicht einzigartig für C, sondern ist eine gemeinsame Funktionalität in vielen Programmierumgebungen aufgrund ihrer Nützlichkeit im Umgang mit flüchtigen Daten. Die `tmpfile()`-Funktion, standardisiert im ISO-C-Standard, erstellt eine Datei mit einem einzigartigen Namen in einem Standardverzeichnis, aber ihre Existenz ist flüchtig, was sie ideal für sichere oder temporäre Operationen macht.

Eine bemerkenswerte Einschränkung von `tmpfile()` ist ihre Abhängigkeit vom Standardtemporärverzeichnis, das möglicherweise nicht für alle Anwendungen geeignet ist, insbesondere in Bezug auf Berechtigungen oder Sicherheit. Im Gegensatz dazu ermöglicht `mkstemp()` die Spezifizierung des Verzeichnisses und stellt die sichere Erstellung von Dateien mit garantiert einzigartigen Dateinamen durch Modifizieren der bereitgestellten Vorlagenzeichenkette sicher, was eine vielseitigere Lösung auf Kosten der manuellen Dateiverwaltung bietet.

Das Erstellen von temporären Dateien kann jedoch Sicherheitsanfälligkeiten einführen, wie z. B. Wettlaufsituationen, wenn es nicht richtig gehandhabt wird. Zum Beispiel adressieren `tmpfile()` und `mkstemp()` unterschiedliche Aspekte der sicheren Erstellung von temporären Dateien (automatische Löschung und sichere Namensgenerierung bzw.), aber keine ist eine Allheillösung. Entwickler müssen die spezifischen Sicherheitsbedürfnisse ihrer Anwendung berücksichtigen, einschließlich potenzieller Sicherheitslücken, die durch temporäre Dateien eingeführt werden könnten, und müssen möglicherweise zusätzliche Sicherungsmechanismen über das hinaus implementieren, was diese Funktionen bieten.

Im breiteren Landschaftsbild der Programmierung könnten Alternativen wie die In-Speicher-Speicherung (z. B. unter Verwendung von dynamischen Datenstrukturen oder speichergemappten Dateien) eine bessere Leistung oder Sicherheit für die Handhabung von temporären Daten bieten. Dennoch bleiben physische temporäre Dateien in vielen Szenarien ein entscheidendes Werkzeug, insbesondere für große Datensätze oder wenn eine Interprozesskommunikation beteiligt ist.
