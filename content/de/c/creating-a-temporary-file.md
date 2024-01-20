---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Erstellen einer temporären Datei in C

## Was & Warum?

Das Erstellen einer temporären Datei ist ein Vorgang, bei dem eine kurzfristig nutzbare Datei in Ihrem System generiert wird. Programmierer machen das oft, um temporäre Daten während der Programmausführung zu speichern, wenn eine dauerhafte Speicherung nicht notwendig ist.

## So geht's:

Erzeugen einer temporären Datei mit der Funktion `tmpfile()`:

```C
#include <stdio.h>
  
int main() 
{ 
    char ch; 
  
    // tmpfile()-Funktion zur Erstellung einer temporären Datei
    FILE* fp = tmpfile(); 
      
    // Prüfung, ob Datei erfolgreich erstellt wurde
    if (fp == NULL) 
    { 
        printf("Datei konnte nicht erstellt werden!\n"); 
        return 0; 
    } 
    else
        printf("Temporäre Datei erfolgreich erstellt!\n"); 

    return 0; 
} 
```

**Ausgabe**

```
Temporäre Datei erfolgreich erstellt!
```

## Vertiefende Informationen

**Historischer Kontext**
Das Erstellen von temporären Dateien hat eine lange Geschichte in der Informatik. Es ermöglichte die Verwaltung großer Mengen von Daten lange vor der Verfügbarkeit von Dauerspeichern wie Festplatten.

**Alternativen**
Es gibt andere Ansätze, um temporäre Daten im Programm zu speichern, z.B. die Verwendung von Datenstrukturen im Speicher. Diese sind jedoch begrenzt durch die Größe des verfügbaren Speichers und eignen sich nicht für sehr große Datenmengen.

**Implementierungsdetails**
Die `tmpfile()`-Funktion erstellt eine einzigartige temporäre Datei im Modus "wb+" und gibt einen Dateizeiger darauf zurück. Die erstellte Datei wird automatisch gelöscht, wenn das Programm beendet wird oder wenn die Datei geschlossen wird.

## Siehe auch

- [tmpfile in der C Standard Library](http://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Stackoverflow Diskussion: Verwendung von temporären Dateien in C](https://stackoverflow.com/questions/1510297/what-is-the-most-secure-way-to-create-a-temporary-file-with-iso-c-and-posix)
- [Verständnis der tmpfile()-Funktion](https://www.geekhideout.com/programming/understanding-the-tmpfile-function.shtml)