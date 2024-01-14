---
title:    "C: Lesen einer Textdatei"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen einer Textdatei ist ein grundlegender Bestandteil des C-Programmierens. Es ermöglicht uns, Daten aus einer Datei zu lesen und sie in unser Programm zu integrieren, anstatt sie manuell einzugeben. Dies spart uns Zeit und sorgt für eine einfachere Verwaltung von Daten.

## So geht's

Um eine Textdatei in C zu lesen, müssen wir zunächst einen Dateizeiger erstellen und die Datei öffnen. Dies geschieht mit der Funktion `fopen()`, die als ersten Parameter den Dateinamen und als zweiten Parameter den gewünschten Lese-/Schreibmodus erhält.

````C
#include <stdio.h>

int main() {

    // Dateizeiger erstellen
    FILE *daten;

    // Datei öffnen und prüfen, ob es erfolgreich war
    if ((daten = fopen("daten.txt", "r")) == NULL) {
        printf("Fehler beim Öffnen der Datei!");
        return 1;
    }

    // Inhalt der Datei lesen und ausgeben
    char inhalt[100];
    while (fgets(inhalt, 100, daten) != NULL) {
        printf("%s", inhalt);
    }

    // Datei schließen
    fclose(daten);

    return 0;
}
````

Die obige Beispielcode liest den Inhalt der Datei "daten.txt" aus und gibt ihn auf der Konsole aus. Der Modus "r" in `fopen()` gibt an, dass die Datei nur gelesen werden soll. Weitere Modi sind zum Beispiel "w" zum Schreiben und "a" zum Anhängen von Daten an eine vorhandene Datei.

## Tieferer Einblick

Ein wichtiger Teil des Lesens einer Textdatei ist das Verständnis der Formatierung des Dateiinhalts. Wir können mit `fscanf()` bestimmte Daten aus der Datei auslesen, aber dafür müssen wir wissen, wie die Daten in der Datei angeordnet sind. Dies kann durch die Verwendung des ASCII-Codes in Kombination mit der Formatierungsspezifikation in `fscanf()` erreicht werden. Zum Beispiel können wir eine Datei mit den folgenden Daten haben:

```
Max Mustermann
25
1.78
75.5
```

Diese Datei enthält den Namen, das Alter, die Körpergröße und das Gewicht einer Person. Wenn wir diese Daten in unser Programm einlesen wollen, können wir dies wie folgt tun:

````C
char name[30];
int alter;
float groesse, gewicht;

fscanf(daten, "%s", name); // liest den Namen aus der Datei aus
fscanf(daten, "%d", &alter); // liest das Alter aus der Datei aus
fscanf(daten, "%f", &groesse); // liest die Körpergröße aus der Datei aus
fscanf(daten, "%f", &gewicht); // liest das Gewicht aus der Datei aus
````

Der Platzhalter `%s` gibt an, dass eine Zeichenkette eingelesen werden soll, während `%d` für eine ganze Zahl und `%f` für eine Fließkommazahl steht.

## Siehe auch

- [C-Programmierung](https://de.wikipedia.org/wiki/C-Programmierung)
- [Dateien in C lesen und schreiben](https://www.programmieraufgaben.ch/aufgabe/dateien-in-c-lesen-und-schreiben/eng6j1ft)