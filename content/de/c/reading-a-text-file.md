---
title:                "C: Das Lesen einer Textdatei"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Textdateien ist ein häufiges Szenario in der Programmierung. Textdateien werden verwendet, um Daten zu speichern, die später von einem Programm gelesen und verarbeitet werden können. Das Lesen von Textdateien ist eine wichtige Fähigkeit, die jeder Programmierer beherrschen sollte.

# Wie es funktioniert

Um eine Textdatei in C zu lesen, müssen wir zunächst die notwendigen Bibliotheken einbinden. Eine schnelle Google-Suche kann Ihnen dabei helfen, die spezifischen Bibliotheken für Ihr Betriebssystem zu finden.

Sobald die notwendigen Bibliotheken eingebunden sind, können wir mit dem eigentlichen Lesen der Datei beginnen. Hier ist ein Beispielcode, der eine Textdatei mit dem Namen "test.txt" liest und ihren Inhalt auf der Konsole ausgibt:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    FILE *datei;
    char c;

    // Öffne die Datei "test.txt" im Lesemodus
    datei = fopen("test.txt", "r");

    // Überprüfe, ob das Öffnen erfolgreich war
    if (datei == NULL)
    {
        printf("Fehler beim Öffnen der Datei!");
        exit(1);
    }

    // Solange das Dateiende noch nicht erreicht ist, lese Zeichen für Zeichen
    while ((c = fgetc(datei)) != EOF)
    {
        // Gib jedes Zeichen auf der Konsole aus
        printf("%c", c);
    }

    // Schließe die Datei
    fclose(datei);

    return 0;
}
```

Das obige Beispiel öffnet die Textdatei "test.txt" im Lesemodus und liest sie Zeichen für Zeichen. Mit dem `fgetc()` Befehl können wir auf das nächste Zeichen in der Datei zugreifen. Die Schleife läuft solange, bis das Dateiende erreicht ist. Der Rückgabewert der `fgetc()` Funktion ist der gelesene Buchstabe als `char` Typ. Dieser wird dann auf der Konsole ausgegeben.

# Tiefergehende Informationen

Das Lesen von Textdateien kann auch komplexer gestaltet werden, indem man zum Beispiel bestimmte Zeichen oder Wörter sucht oder die Datei Schritt für Schritt durchsucht. Dafür gibt es verschiedene Funktionen in C, wie zum Beispiel `fgets()` oder `fscanf()`, die nützlich sein können.

Außerdem müssen wir beachten, dass während des Lesens von Dateien möglicherweise Fehler auftreten können. Daher ist es wichtig, die Funktion `ferror()` zu verwenden, um zu prüfen, ob ein Fehler aufgetreten ist.

Es gibt auch verschiedene Möglichkeiten, Textdateien zu schreiben, zu bearbeiten und zu löschen, die im Zusammenhang mit dem Lesen wichtig sein können. Eine gründlichere Auseinandersetzung mit diesen Themen kann daher von Vorteil sein.

# Siehe auch

- [C-Programmierungstutorial (auf Deutsch)](https://www.c-howto.de/)
- [Bibliotheksreferenz für die C-Programmierung](https://www.cplusplus.com/reference/cstdio/) (Englisch)