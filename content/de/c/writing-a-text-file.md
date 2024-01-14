---
title:    "C: Eine Textdatei schreiben."
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien sind ein grundlegender Bestandteil jeder Programmiersprache und ein unverzichtbares Werkzeug für Entwickler. Sie ermöglichen es uns, Daten zu speichern, zu lesen und zu bearbeiten, was für viele Programme unerlässlich ist. In diesem Blog-Beitrag werden wir uns genauer mit dem Schreiben von Textdateien in C beschäftigen und zeigen, wie einfach es sein kann.

## Wie geht das?

Das Schreiben einer Textdatei in C ist relativ unkompliziert. Wir müssen lediglich einige Schritte befolgen:

1. Eine Datei mit dem `fopen()` Befehl öffnen.
2. Den Text in die Datei schreiben mit `fprintf()`.
3. Die Datei schließen mit `fclose()`.

Schauen wir uns nun ein Beispiel an, wie wir das in C implementieren können.

```C
#include <stdio.h>

int main()
{
  // Datei mit fopen öffnen
  FILE *fptr = fopen("textdatei.txt", "w");

  // Überprüfen, ob die Datei erfolgreich geöffnet wurde
  if (fptr == NULL)
  {
    printf("Fehler beim Öffnen der Datei!");
    return 0;
  }

  // Text in die Datei schreiben mit fprintf
  fprintf(fptr, "Das ist eine Beispieltextdatei!");

  // Datei schließen
  fclose(fptr);

  printf("Text wurde erfolgreich in die Datei geschrieben.");
  
  return 0;
}
```

Wenn wir dieses Programm ausführen, wird eine neue Datei namens "textdatei.txt" erstellt und der angegebene Text wird in die Datei geschrieben. Wenn wir die Datei nun öffnen, werden wir sehen, dass der Text erfolgreich geschrieben wurde.

## Tiefergehende Informationen

Beim Schreiben von Textdateien in C gibt es einige wichtige Dinge zu beachten:

- Beim Öffnen der Datei müssen wir den richtigen Modus angeben (`w` für das Schreiben).
- Wenn die Datei bereits existiert, wird sie überschrieben.
- Wir können den Inhalt der Datei durch Hinzufügen von `a` zum Modus ergänzen (z.B. `a+`).
- Mithilfe von `fprintf()` können wir auch Variablen in die Datei schreiben, indem wir dem Text Platzhalter wie `%d` oder `%f` hinzufügen.

Es ist auch wichtig, die Datei immer mit `fclose()` zu schließen, um eventuelle Datenverluste zu vermeiden.

## Siehe auch

- [C-Programmierung – Einführung zu Dateien](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-Bibliotheksfunktion - fopen()](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [C-Bibliotheksfunktion - fprintf()](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)