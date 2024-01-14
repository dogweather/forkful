---
title:                "C: Textdatei lesen"
simple_title:         "Textdatei lesen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Textdateien sind ein häufig verwendeter Datentyp in der Programmierung, insbesondere in der C-Programmierung. Das Lesen von Textdateien kann eine nützliche Fähigkeit sein, um Daten aus externen Quellen in Ihr Programm zu integrieren. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man Textdateien in C liest.

## Wie geht es
Um eine Textdatei in C zu lesen, sind einige Schritte erforderlich. Zuerst müssen Sie eine Verbindung zur Datei herstellen, dann müssen Sie die Datei öffnen und schließlich müssen Sie den Inhalt der Datei lesen. Wir werden diese Schritte im Folgenden detailliert beschreiben.

```C
#include <stdio.h>
int main()
{
    // Datei-Zeiger erstellen
    FILE *datei;

    // Datei öffnen
    datei = fopen("beispiel.txt", "r");

    // Überprüfen, ob Datei geöffnet wurde
    if (datei == NULL)
    {
        printf("Fehler beim Öffnen der Datei!");
        return 1;
    }

    // Variablen zum Lesen der Datei erstellen
    char zeile[100];

    // Datei zeilenweise lesen und ausgeben
    while (fgets(zeile, 100, datei) != NULL)
    {
        printf("%s", zeile);
    }

    // Datei schließen
    fclose(datei);

    return 0;
}
```

In diesem Beispiel öffnen wir eine Textdatei namens "beispiel.txt" und lesen sie zeilenweise mit der Funktion `fgets()`. Die `while`-Schleife wird so lange ausgeführt, bis das Ende der Datei erreicht ist. Schließlich wird die Datei mit `fclose()` wieder geschlossen.

Die Ausgabe für diese Datei könnte so aussehen:

```
Das ist Zeile 1
Das ist Zeile 2
Das ist Zeile 3
```

## Eine tiefergehende Erklärung
Beim Lesen einer Textdatei in C gibt es einige wichtige Konzepte zu beachten. Der erste ist die Verwendung von `FILE`-Pointern, die es uns ermöglichen, eine Verbindung zu einer Datei herzustellen. Der zweite ist die Verwendung von Zeichenarrays für das Lesen von Daten aus der Datei. Und schließlich gibt es einige Funktionen, die wir verwenden können, um Daten aus der Datei zu lesen, wie zum Beispiel `fgets()`, `fscanf()` und `getc()`.

Es ist auch wichtig zu beachten, dass beim Öffnen einer Datei zwei erste Argumente in `fopen()` verwendet werden. Der erste ist der Dateiname und der zweite gibt den Modus an, in dem die Datei geöffnet wird. In unserem Beispiel verwenden wir "r" für den Lesezugriff auf die Datei, aber es gibt auch andere Modi wie "w" für den Schreibzugriff oder "a" für das Anhängen von Daten an eine Datei.

Für eine detailliertere Erklärung der einzelnen Funktionen und Konzepte empfehle ich Ihnen, weitere Ressourcen zu konsultieren, die am Ende dieses Artikels aufgeführt sind.

## Siehe auch
- [C-Programmierung: Lesen von Dateien](https://www.studytonight.com/c/file-handling-in-c.php)
- [C-Programmierung: Dateien öffnen und schließen](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-Dokumentation: Funktionen zum Lesen von Dateien](https://en.cppreference.com/w/c/io/fopen)