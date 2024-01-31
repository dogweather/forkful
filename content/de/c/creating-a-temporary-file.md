---
title:                "Erstellen einer temporären Datei"
date:                  2024-01-20T17:39:35.938282-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellen einer temporären Datei"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Das Erstellen einer temporären Datei ermöglicht das sichere Speichern von Daten während der Laufzeit eines Programms. Programmierer nutzen dies für Datenaustausch, Caching oder um Arbeitsspeicher zu sparen.

## How to:
C bietet mehrere Funktionen, um temporäre Dateien zu handhaben. Hier ist ein einfaches Beispiel mit `tmpfile()`:

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp) {
        fputs("Dies ist ein Test.\n", temp);

        // Spule zurück zum Dateianfang, um das Geschriebene zu lesen
        rewind(temp);

        char buffer[100];
        while (fgets(buffer, sizeof(buffer), temp)) {
            printf("Gelesen: %s", buffer);
        }

        // Das Schließen der Datei löscht sie automatisch
        fclose(temp);
    } else {
        perror("tmpfile() fehlgeschlagen");
    }

    return 0;
}
```
Ausgabe:
```
Gelesen: Dies ist ein Test.
```

## Deep Dive
Vor der Einführung von `tmpfile()` und ähnlichen Funktionen, mussten Entwickler eigene Mechanismen implementieren, um temporäre Dateien zu verwenden, was oft unsicher war. 

`tmpfile()` erstellt eine eindeutige Datei und öffnet sie im Modus "wb+". Die Datei wird automatisch gelöscht, wenn sie geschlossen wird oder das Programm endet.

Es gibt Alternativen wie `mkstemp()`, die sicherer sein kann, da sie dem Benutzer mehr Kontrolle gibt. Mit `mkstemp()` muss man jedoch selbst für die Löschung der Datei sorgen.

## See Also
- C Standardbibliothek Dokumentation: https://en.cppreference.com/w/c/io/tmpfile
- Linux Manual Page für `mkstemp()`: https://man7.org/linux/man-pages/man3/mkstemp.3.html
