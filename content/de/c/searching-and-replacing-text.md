---
title:                "C: Suchen und Ersetzen von Text"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Suchen und Ersetzen von Text ist eine grundlegende Funktion beim Programmieren, die dabei hilft, unerwünschte oder veraltete Informationen zu aktualisieren oder zu entfernen. Mit C-Programmierung können Sie effizient ganze Dateien oder Teilabschnitte nach bestimmten Wörtern oder Zeichenketten durchsuchen und diese durch neue Informationen ersetzen.

## Wie funktioniert es?

Die Suche und Ersetzungsfunktion in C basiert auf der Verwendung von Zeichenkettenfunktionen wie "strstr", die eine bestimmte Zeichenkette in einer anderen suchen und die Position des ersten Vorkommens zurückgeben. Um eine Zeichenkette in einer Datei zu ersetzen, kann die Funktion "sprintf" verwendet werden, um die neue Zeichenkette zusammen mit der alten Zeichenkette in eine temporäre Variable zu speichern. Anschließend kann die Funktion "fputs" verwendet werden, um den Inhalt der temporären Variable in die eigentliche Datei zu schreiben.

Lassen Sie uns anhand eines Beispiels genauer betrachten, wie das Suchen und Ersetzen von Text in C funktioniert:

```C
#include <stdio.h>

int main()
{
    // Öffnen der Datei im Lesemodus
    FILE *fp = fopen("test.txt", "r");
    // Öffnen einer neuen Datei im Schreibmodus
    FILE *new_fp = fopen("neue_datei.txt", "w");
    // Suchen und Ersetzen aller Instanzen von "Hallo" mit "Guten Tag"
    char search_string[] = "Hallo";
    char replace_string[] = "Guten Tag";
    // Buffer zum Speichern der temporären Zeichenkette
    char temp[1000];

    while (!feof(fp))
    {
        // Lesen der Datei bis zum ersten Vorkommen von search_string
        // und Speichern des Inhalts in temp
        fscanf(fp, "%[^\n]s", temp);
        // Suchen nach search_string in temp
        char *position = strstr(temp, search_string);
        // Wenn found, dann ersetzen mit replace_string und schreiben in neue_datei.txt
        if (position != NULL)
        {
            // Verwenden von sprintf, um die neue Zeichenkette in temp zu speichern
            sprintf(temp, "%s%s", replace_string, (position + strlen(search_string)));
            // Schreiben von temp in neue_datei.txt
            fputs(temp, new_fp);
        }
        else
        {
            // Ansonsten einfach den Inhalt von temp in neue_datei.txt schreiben
            fputs(temp, new_fp);
        }
    }
    // Schließen der Dateien
    fclose(fp);
    fclose(new_fp);
    return 0;
}
```

Mit diesem Beispiel können alle Instanzen von "Hallo" in der Datei "test.txt" durch "Guten Tag" ersetzt werden.

## Tiefergehende Informationen

Beim Suchen und Ersetzen von Text in C ist es wichtig, einige wichtige Funktionen wie "feof", "fscanf", "fgets" und "fputs" zu verstehen. "feof" wird verwendet, um festzustellen, ob das Dateiende erreicht wurde, während "fscanf" und "fgets" zum Lesen von Zeichen, Wörtern oder Zeilen aus einer Datei verwendet werden. "fputs" hingegen wird zum Schreiben von Zeichenketten in eine Datei verwendet.

Es ist auch wichtig zu beachten, dass Suchen und Ersetzen in C case-sensitive ist, was bedeutet, dass "Hallo" nicht mit "hallo" oder "HALLO" ersetzt wird. Für nicht case-sensitive Suchanfragen können die Funktionen "strcasestr" und "strncasecmp" verwendet werden.

## Siehe auch

- [Die offizielle Dokumentation von C](https://devdocs.io/c/)
- [Tutorial zum Suchen und Ersetzen von Text in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Weitere nützliche Zeichenkettenfunktionen in C](https://www.geeksforgeeks.org/string-handling-cc/)