---
title:    "C: Erstellen einer temporären Datei"
keywords: ["C"]
---

{{< edit_this_page >}}

## Warum
In der Programmierung ist es oft notwendig, temporäre Dateien zu erstellen, um Daten zu speichern oder zu verarbeiten, die während der Laufzeit des Programms benötigt werden. Diese temporären Dateien werden normalerweise gelöscht, sobald das Programm beendet wird.

## Wie man temporäre Dateien in C erstellt

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
   // Erstellt eine temporäre Datei mit dem Präfix "temp"
   // und speichert den Dateinamen in der Variablen "tempfile"
   char tempfile[] = "tempXXXXXX";
   FILE *fp = mkstemp(tempfile);

   if (fp == NULL) {
      printf("Fehler beim Erstellen der temporären Datei!");
      exit(1);
   }else{
      printf("Temporäre Datei '%s' erfolgreich erstellt.", tempfile);
   }

   // Schreibt etwas in die temporäre Datei
   fputs("Dies ist ein Text in der temporären Datei.", fp);

   // Schließt die Datei und löscht sie
   fclose(fp);
   remove(tempfile);

   return 0;
}
```

Die oben gezeigte Beispielcode erstellt eine temporäre Datei mit dem Präfix "temp" und einem zufällig generierten Suffix. Der Dateiname wird dann in der Variablen "tempfile" gespeichert. Mit der Funktion `mkstemp()` wird die Datei erstellt und ein Dateideskriptor wird zurückgegeben. Wenn ein Fehler auftritt, wird dies überprüft und das Programm beendet. Ansonsten wird der Dateiname ausgegeben und ein Beispieltext in die temporäre Datei geschrieben. Zum Schluss wird die Datei geschlossen und mit der Funktion `remove()` gelöscht.

## Tiefer Einblick
Es gibt noch weitere Möglichkeiten, temporäre Dateien in C zu erstellen. Eine andere Möglichkeit ist die Verwendung der Funktion `tmpnam()`, die einen zufälligen, noch nicht existierenden Dateinamen zurückgibt. Jedoch ist diese Funktion nicht sehr sicher in der Verwendung, da es möglich ist, dass ein anderer Prozess bereits dieselbe Datei erstellt hat.

Eine weitere Möglichkeit ist die Verwendung der Funktion `tmpfile()`, die einen Dateideskriptor einer temporären Datei zurückgibt. Der Nachteil ist, dass diese Datei im Arbeitsspeicher erstellt wird und keine Möglichkeit besteht, sie auf der Festplatte zu speichern.

## Siehe auch
- [Dokumentation zu mkstemp()](https://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- [Beispiele für die Verwendung von temporären Dateien in C](https://www.geeksforgeeks.org/temporary-file-creation-in-c/)
- [Weitere Informationen zu temporären Dateien in C](https://c-for-dummies.com/blog/?p=3131)