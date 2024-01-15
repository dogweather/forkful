---
title:                "Ein temporäres Datei erstellen"
html_title:           "C: Ein temporäres Datei erstellen"
simple_title:         "Ein temporäres Datei erstellen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt die Mühe machen, eine temporäre Datei mit C zu erstellen? Nun, temporäre Dateien sind nützliche Werkzeuge, um Daten zwischen verschiedenen Programmen oder Prozessen auszutauschen. Sie dienen auch als vorübergehender Speicher, um Daten zu speichern, die später wieder gelöscht werden sollen.

## Wie es geht
Um eine temporäre Datei mit C zu erstellen, müssen wir die Standardbibliothek `stdio.h` einbinden und die Funktion `tmpfile()` verwenden. Hier ist ein Beispielcode:

```C
#include<stdio.h>

int main()
{
    FILE *temp_file;
    temp_file = tmpfile();
    if (temp_file == NULL)
    {
        printf("Fehler beim Erstellen der temporären Datei!");
        return 1;
    }
    printf("Temporäre Datei erfolgreich erstellt!");
    return 0;
}
```

Hier verwenden wir die `tmpfile()` Funktion, um eine neue temporäre Datei zu erstellen und weisen sie der Variable `temp_file` zu. Dann überprüfen wir, ob die Datei erfolgreich erstellt wurde und geben entweder eine Fehlermeldung oder eine Bestätigung aus. Vergiss nicht, die temporäre Datei zu schließen und zu löschen, wenn du sie nicht mehr benötigst.

## Tiefer Einblick
Um besser zu verstehen, wie die `tmpfile()` Funktion funktioniert, werfen wir einen kurzen Blick auf den Prozess der Erstellung einer temporären Datei. C verwendet intern eine spezielle Struktur, um temporäre Dateien zu erstellen und zu verwalten. Diese Struktur speichert Informationen über die Daten und den Speicherort der temporären Datei.

Die `tmpfile()` Funktion erstellt dann eine temporäre Datei in einem temporären Ordner auf deinem System. Der genaue Speicherort hängt von deinem Betriebssystem und der Konfiguration ab. Sobald die Datei geschlossen oder das Programm beendet wird, wird sie automatisch gelöscht.

## Siehe auch
* Mehr über temporäre Dateien in C erfahren: https://www.geeksforgeeks.org/temporary-files-in-c-programming/
* Weitere Funktionen in `stdio.h`: https://www.tutorialspoint.com/c_standard_library/stdio_h.htm