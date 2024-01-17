---
title:                "Löschen von Zeichen, die einem Muster entsprechen."
html_title:           "C: Löschen von Zeichen, die einem Muster entsprechen."
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine häufige Aufgabe in der C-Programmierung. Programmierer nutzen dies, um beispielsweise unerwünschte Zeichen in einer Zeichenkette zu entfernen oder bestimmte Muster zu erkennen und zu bearbeiten.

## So geht's:
Um in C Zeichen zu löschen, die einem bestimmten Muster entsprechen, können wir die eingebaute Funktion `strchr` verwenden. Diese Funktion durchsucht eine gegebene Zeichenkette nach einem bestimmten Zeichen und gibt den Index des ersten gefundenen Vorkommnisses zurück. Mit dieser Information können wir dann die entsprechenden Zeichen löschen. Ein Beispiel dafür könnte so aussehen:

```C
#include <stdio.h>
#include <string.h>

int main () {
  char string[] = "Hallo Welt!";
  char *p = strchr(string, 'l');
  
  if (p) {
    strcpy(p, p+1); // Zeichen an gefundener Stelle und das folgende überschreiben
  }
  
  printf("%s\n", string);
  
  return 0;
}

/* Ausgabe:
Hao Welt!
*/
```

## Tiefergehend:
Das Löschen von Zeichen wurde in der C-Programmierung schon seit langer Zeit verwendet, um beispielsweise die Ausgabe von Benutzereingaben zu bereinigen oder um spezielle Zeichen in Dateien zu erkennen und zu bearbeiten. Alternativ zur `strchr`-Funktion gibt es auch die Funktionen `strrchr` und `strpbrk`, welche ähnliche Aufgaben erfüllen, jedoch etwas anders arbeiten. 

Bei der Implementierung von Funktionen zum Löschen von Zeichen sollte darauf geachtet werden, dass der Speicher korrekt verwaltet wird und keine unbeabsichtigten Seiteneffekte auftreten. Es gibt auch spezielle Aspekte zu beachten, wenn es um die Verarbeitung von Unicode-Zeichen geht, welche über die Standard-ASCII-Zeichen hinausgehen.

## Sieh auch:
- [Dokumentation der `strchr`-Funktion](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)
- [Weitere nützliche String-Funktionen in C](https://www.thecrazyprogrammer.com/2015/08/string-functions-in-c-programming.html)