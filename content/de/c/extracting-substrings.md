---
title:    "C: Extrahieren von Teilstrings"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilzeichenketten ist eine wichtige Fähigkeit im C-Programmieren. Es ermöglicht Ihnen, Teile einer Zeichenkette zu isolieren und für spezifische Aufgaben zu verwenden. Zum Beispiel könnten Sie nur den Vornamen einer Person aus einer vollständigen Namen-Zeichenkette extrahieren, um sie in einer Begrüßungsnachricht anzuzeigen. Das Extrahieren von Teilzeichenketten gibt Ihnen mehr Kontrolle über die Daten, mit denen Sie arbeiten, und macht Ihr Programm effizienter und lesbarer.

## Wie geht's

Um eine Teilzeichenkette in C zu extrahieren, können Sie die Funktion "strncpy" verwenden. Diese Funktion kopiert eine bestimmte Anzahl von Zeichen aus einer Zeichenkette in eine andere Zeichenkette. Hier ist ein Beispielcode, der den Vornamen aus einer vollständigen Namen-Zeichenkette extrahiert und ihn in einer separaten Variablen speichert:

```C
#include <stdio.h>
#include <string.h>

int main() {
  char name[] = "Max Mustermann";
  char first_name[6]; // speichert maximal 5 Zeichen (plus Null-Terminierung)

  strncpy(first_name, name, 5); // kopiert 5 Zeichen von name nach first_name
  first_name[5] = '\0'; // fügt Null-Terminierung an
  printf("Hallo %s!", first_name);

  return 0;
}
```

Die Ausgabe dieses Codes wäre "Hallo Max!", da der Vornamen aus der Namen-Zeichenkette extrahiert wurde und in der neuen Variablen gespeichert ist. Beachten Sie, dass die Funktion "strncpy" auch die Null-Terminierung automatisch anfügt, wenn die Anzahl der kopierten Zeichen kleiner ist als die Größe der Zielvariablen.

## Tiefergehende Informationen

Die Funktion "strncpy" hat einige Besonderheiten, die Sie beim Extrahieren von Teilzeichenketten beachten sollten. Zum Beispiel kopiert sie immer die angegebene Anzahl von Zeichen, auch wenn die Quellen-Zeichenkette kürzer ist. Wenn Sie also versuchen, 10 Zeichen aus einer 5-Zeichen-Zeichenkette zu kopieren, wird die Null-Terminierung an die 6. Stelle der Zielvariablen angefügt.

Außerdem sollten Sie beachten, dass die Funktion "strncpy" keine Überlappungen zwischen den Quellen- und Zielvariablen zulässt. Wenn Sie also versuchen, Zeichen aus einer Zeichenkette in die gleiche Zeichenkette zu kopieren (z.B. "strncpy(name, name + 5, 5)"), wird das Verhalten nicht definiert.

Um mögliche Probleme zu vermeiden, können Sie die Funktion "strlcpy" verwenden, die in der Bibliothek "string.h" verfügbar ist. Diese Funktion behandelt Überlappungen und sorgt dafür, dass die Zielvariable immer richtig abgeschlossen wird.

## Siehe auch

- [Dokumentation zu strncpy (auf Englisch)](https://en.cppreference.com/w/c/string/byte/strncpy)
- [Dokumentation zu strlcpy (auf Englisch)](https://man.openbsd.org/strlcpy)
- [Weitere Funktionen zum Manipulieren von Zeichenketten in C (auf Deutsch)](https://www.cplusplus.com/reference/cstring/)