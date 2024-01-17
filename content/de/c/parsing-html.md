---
title:                "Das Parsen von HTML"
html_title:           "C: Das Parsen von HTML"
simple_title:         "Das Parsen von HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-html.md"
---

{{< edit_this_page >}}

## Was ist das und warum machen wir es?
Das Parsen von HTML ist die Aufgabe eines Programms, um die Struktur von HTML-Dateien zu analysieren und die enthaltenen Informationen zu extrahieren. Programmierer verwenden das Parsen von HTML, um beispielsweise Webseiten zu scrapen oder um Strukturen von XML-Dokumenten zu analysieren.

## So geht's:
Ein Beispiel, wie man HTML in C parsen kann:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LENGTH 100

int main() {
  char html[MAX_LENGTH] = "<html><head><title>Beispiel</title></head><body><h1> Willkommen </h1></body></html>";
  char *tag;
  char *title;

  // Sucht nach dem <title> Tag
  tag = strstr(html, "<title>");
  // Sucht nach dem dazugehörigen </title> Tag
  title = strstr(tag, "</title>");

  // Speichert den Titel in einer neuen Zeichenkette
  char page_title[50];
  strncpy(page_title, tag + 7, title - tag - 7);
  page_title[title - tag - 7] = '\0';

  printf("Der Titel der Seite lautet: %s\n", page_title);

  return 0;
}
```

## Tiefentauchen:
Das Parsen von HTML ist seit den Anfängen des World Wide Web im Jahr 1991 ein wichtiger Bestandteil der Webentwicklung. Es gibt verschiedene Methoden und Algorithmen, um HTML zu parsen, wie z.B. Recursive descent oder Regular Expressions. Eine Alternative zum Parsen ist das Verwenden von bereits existierenden Bibliotheken oder Frameworks, die das Parsen von HTML erleichtern. Bei der Implementierung sollten auch mögliche Fehlerquellen, wie mögliche unerwartete Tags oder fehlerhafte Strukturen, berücksichtigt werden.

## Siehe auch:
- [https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/HTML_Parsing](https://developer.mozilla.org/de/docs/Web/JavaScript/Guide/HTML_Parsing)
- [https://www.w3.org/html/](https://www.w3.org/html/)
- [https://www.w3schools.com/jsref/met_doc_getelementsbyclassname.asp](https://www.w3schools.com/jsref/met_doc_getelementsbyclassname.asp)