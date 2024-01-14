---
title:    "C: Löschen von Zeichen, die einem Muster entsprechen."
keywords: ["C"]
---

{{< edit_this_page >}}

# Warum
Das Löschen von Zeichen basierend auf einem bestimmten Muster ist eine häufig vorkommende Aufgabe in der Programmierung. Es kann helfen, Daten zu filtern und zu bereinigen oder auch das Erscheinungsbild von Texten oder Benutzereingaben zu verbessern. Daher ist es wichtig zu verstehen, wie man dieses Problem effektiv in C lösen kann.

## How To
Eine gängige Methode, um Zeichen nach einem bestimmten Muster zu löschen, ist die Verwendung von Schleifen und bedingten Anweisungen. Im Folgenden ist ein Beispielcode dargestellt, der alle Ziffern aus einer Zeichenkette entfernt:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char eingabe[100];
    printf("Geben Sie eine Zeichenkette ein: ");
    scanf("%s", eingabe);

    int laenge = strlen(eingabe);

    // Schleife, die alle Zeichen durchläuft
    for (int i = 0; i < laenge; i++) {

        // Bedingung, um zu prüfen, ob es sich um eine Ziffer handelt
        if (eingabe[i] >= '0' && eingabe[i] <= '9') {

            // Verschiebe alle nachfolgenden Zeichen einen Platz nach vorne
            for (int j = i; j < laenge; j++) {
                eingabe[j] = eingabe[j + 1];
            }
            // Reduziere die Länge der Zeichenkette um eins,
            // da ein Zeichen entfernt wurde
            laenge--;
            // Stelle sicher, dass das aktuelle Zeichen nochmal überprüft wird
            i--;
        }
    }
    printf("Ergebnis: %s\n", eingabe);
    return 0;
}
```
**Ausgabe:**

Geben Sie eine Zeichenkette ein: Hallo1234Welt
Ergebnis: HalloWelt

In diesem Beispiel wird die Zeichenkette eingelesen und anschließend mit einer Schleife vollständig durchlaufen. Wenn ein Zeichen eine Ziffer ist, wird es verschoben und die Länge der Zeichenkette um eins reduziert. Dadurch wird das entsprechende Zeichen gelöscht. Am Ende wird das Ergebnis ausgegeben.

## Deep Dive
Man kann auch die Funktion `memmove()` aus der Standardbibliothek `string.h` verwenden, um eine Zeichenkette zu manipulieren. Diese Funktion ist jedoch etwas komplizierter zu verwenden, da sie nicht nur einen Teil der Zeichenkette, sondern die gesamte Zeichenkette verschieben kann. Daher ist es wichtig, sorgfältig mit den Argumenten umzugehen.

Ein weiterer wichtiger Aspekt ist das Verständnis von Zeigerarithmetik. Um die Verschiebung von Zeichenkette zu Zeichenkette zu ermöglichen, werden Zeiger verwendet, die auf den Anfang und das Ende der Zeichenkette verweisen. Durch die Verwendung von Zeigerarithmetik kann man dann einzelne Zeichen aus der Zeichenkette auswählen und manipulieren.

Die Verwendung von Funktionen aus der Standardbibliothek ist in der Regel effizienter und einfacher zu implementieren als manuelle Lösungen. Allerdings ist es wichtig, sich mit den Funktionen und Konzepten vertraut zu machen, um sie erfolgreich anwenden zu können.

## Siehe auch
- [Offizielle Dokumentation für die Funktion `memmove()` (auf Englisch)](https://www.man7.org/linux/man-pages/man3/memmove.3.html)
- [Beispielcode für den Einsatz von `memmove()` in C (auf Englisch)](https://www.programiz.com/c-programming/library-function/string.h/memmove)