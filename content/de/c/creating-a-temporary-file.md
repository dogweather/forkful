---
title:                "Erstellen einer temporären Datei"
html_title:           "C: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Was & Warum?

Temporäre Dateien sind Dateien, die von Programmen erstellt werden, um vorübergehende Daten zu speichern. Programmierer nutzen sie oft, um Zwischenergebnisse oder temporäre Variablen zu speichern. Diese Dateien werden normalerweise gelöscht, wenn das Programm beendet wird.

# Wie geht's weiter?

Die Erstellung einer temporären Datei ist relativ einfach. Der folgende Code zeigt, wie dies in C gemacht werden kann:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // temporäre Datei erstellen
    FILE *tempFile = tmpfile();

    // Überprüfen, ob die Datei erfolgreich erstellt wurde
    if (tempFile == NULL) {
        printf("Fehler beim Erstellen der temporären Datei\n");
        return 1;
    }

    // Schreibzugriff auf die Datei
    fprintf(tempFile, "Das ist eine Testdatei\n");
    fprintf(tempFile, "Sie wird bald gelöscht\n");

    // Schließen und löschen der temporären Datei
    fclose(tempFile);

    // Ausgabe der Datei
    printf("Inhalt der temporären Datei:\n");
    system("cat /tmp/tmp*");

    return 0;
}
```

Der oben stehende Code erstellt eine temporäre Datei mit Hilfe der Funktion `tmpfile()` aus der Standardbibliothek `stdio.h`. Anschließend wird die Datei mit `fprintf()` beschrieben und nach Beendigung des Programms über die Funktion `fclose()` gelöscht.

Wenn das Programm erfolgreich ausgeführt wird, wird der Inhalt der temporären Datei mit dem Befehl `cat` angezeigt. Der Output sollte folgendermaßen aussehen:

```
Inhalt der temporären Datei:
Das ist eine Testdatei
Sie wird bald gelöscht
```

# Tiefere Einblicke

Die Verwendung von temporären Dateien ist eine häufig verwendete Methode in der Programmierung. Sie werden oft als Alternative zu anderen Speicherungen wie z.B. in Arrays oder Variablen verwendet. Zusätzlich dienen sie auch als eine Art Sicherungskopie für Daten, die während der Ausführung des Programms nicht benötigt werden.

Die Funktion `tmpfile()` wird seit der Version C89 im Standard der Programmiersprache C aufgeführt. Es gibt jedoch auch andere Methoden, um temporäre Dateien zu erstellen. Zum Beispiel kann man die Funktion `mkstemp()` verwenden, die mehr Kontrolle über den Dateinamen bietet.

Es ist wichtig, die erstellten temporären Dateien ordnungsgemäß zu schließen und zu löschen, um Speicherlecks oder ungewollte Speicherplatzbelegungen zu vermeiden.

# Siehe auch

- [C-Befehlsreferenz für tmpfile()](http://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Alternative: mkstemp() Funktion](http://www.cplusplus.com/reference/cstdlib/mkstemp/)