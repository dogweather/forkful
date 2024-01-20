---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

---

## Was & Warum?

"Den aktuellen Tag abrufen" bezieht sich auf die Gewinnung des aktuellen Datums aus dem System. Es ist nützlich, um eine datumsabhängige Funktionalität zu implementieren, wie z.B. Logbuchaufzeichnungen oder Zeitstempelung von Daten.

---

## Wie macht man das:

Nehmen wir ein leichtes Beispiel, um das Konzept besser zu verdeutlichen. Unten finden Sie den C-Code, mit dem Sie das aktuelle Datum abrufen können.

```C
#include <time.h>
#include <stdio.h>

int main(){
   time_t t = time(NULL);
   struct tm tm = *localtime(&t);
   printf("heute ist %02d.%02d.%d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
   return 0;
}
```
Wenn Sie das Programm laufen lassen, wird es das heutige Datum in "DD.MM.YYYY" Format ausgeben.

---

## Deep Dive:

Historisch gesehen hatte jedes Betriebssystem seine eigenen Wege, um das aktuelle Datum zu erhalten und die Zeit zu verwalten. Einfache, portable Lösungen wurden erst in neuerer Zeit mit der Verbreitung der C Standard Bibliothek erhältlich.

Es gibt verschiedene Möglichkeiten, um an das aktuelle Datum zu kommen, abhängig von den spezifischen Anforderungen und der zur Verfügung stehenden Bibliothek. Beispielsweise können wir auch die `strftime`-Methode verwenden, um das aktuelle Datum in einem bestimmten Format zu bekommen:

```C
#include <time.h>
#include <stdio.h>

int main() {
   time_t t = time(NULL);
   struct tm *tm = localtime(&t);
   char date[64];
   strftime(date, sizeof(date), "%d.%m.%Y", tm);
   printf("heute ist %s\n", date);
   return 0;
}
```
Diese Methode ermöglicht eine detailliertere Steuerung des Formatierungsausdrucks.

Die Implementierungsdetails hinter dieser Funktion sind ziemlich tiefgreifend und gehen über den Rahmen dieses Artikels hinaus. Die `localtime`-Funktion konvertiert die durch `time()` zurückgegebene Zeit in eine `tm`-Struktur, die den Zeitwert in die lokal gültige Zeit umwandelt.

---

## Siehe auch

Fügen Sie hier Links zu relevanten Ressourcen oder verwandten Themen ein, um dem Leser eine umfassende Anleitung zu bieten. Hier sind einige Empfehlungen:

- [C Library - <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [strftime Funktion in C](https://www.cplusplus.com/reference/ctime/strftime/)
- StackOverflow [thread on `localtime`](https://stackoverflow.com/questions/5141960/get-the-current-time-in-c)