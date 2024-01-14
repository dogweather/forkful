---
title:                "C: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum
Für viele Programmierer ist das Erstellen von temporären Dateien eine gängige Praxis. Diese Dateien werden für eine Vielzahl von Zwecken verwendet, wie zum Beispiel zum Speichern von Zwischenergebnissen, zur Fehlerbehebung oder zum Testen von Code. Sie sind nützlich, weil sie schnell erstellt und gelöscht werden können, ohne dass der Prozess des eigentlichen Programms beeinträchtigt wird.

# Wie man es macht
Um eine temporäre Datei in C zu erstellen, können Sie die Funktion ```tmpfile()``` verwenden. Diese Funktion erstellt eine temporäre Datei und gibt einen Zeiger auf die Datei zurück, über den Sie darauf zugreifen können. Schauen wir uns ein Beispiel an:

```
#include <stdio.h>

int main() {
  FILE *tmp;
  tmp = tmpfile();
  fprintf(tmp, "Dies ist eine temporäre Datei.");
  rewind(tmp);
  char str[50];
  fscanf(tmp, "%s", str);
  printf("%s", str);
  fclose(tmp);
  return 0;
}

```

Dieser Code erstellt eine temporäre Datei, fügt einen Text hinzu, liest den Text in eine Variable und gibt den Text dann auf der Konsole aus. Der Output des Programms wird folgendermaßen aussehen: 

```
Dies ist eine temporäre Datei.
```

# Eintauchen
Wenn Sie tiefer in das Thema der temporären Dateien einsteigen möchten, sollten Sie sich mit den verschiedenen Funktionen auseinandersetzen, die zum Erstellen und Löschen von temporären Dateien verwendet werden können. Zum Beispiel existiert auch die Funktion ```mkstemp()```, die ähnliche Ergebnisse erzielt wie ```tmpfile()```, aber Ihnen mehr Kontrolle über die Datei gibt, da sie einen Dateinamen zurückgibt, den Sie verwenden können.

Eine weitere wichtige Sache, die man beachten sollte, ist, dass temporäre Dateien nicht nur in C, sondern auch in anderen Programmiersprachen wie Java oder Python häufig verwendet werden. In diesen Sprachen existieren spezifische Funktionen zum Erstellen von temporären Dateien, die unterschiedliche Ergebnisse erzielen können.

# Siehe auch
- [Einführung in temporäre Dateien in C](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [Verwendung von temporären Dateien in Python](https://www.geeksforgeeks.org/python-temporary-files/)