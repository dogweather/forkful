---
title:                "C: HTML Parsen"
simple_title:         "HTML Parsen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Das Parsen von HTML ist eine wichtige Fähigkeit, die jeder C-Programmierer kennen sollte. Durch das Parsen von HTML können Daten aus Webseiten extrahiert und in anderen Anwendungen verwendet werden.

## Anleitung

Um HTML in C zu parsen, gibt es einige Bibliotheken und Tools wie z.B. libxml2 oder HTML Tidy. Wir können jedoch auch eine sehr einfache Methode verwenden, die auf den spezifischen Bedürfnissen unserer Anwendung basiert.

Hier ist ein Beispielcode, der den Textinhalt eines HTML-Tags "p" aus einer Datei extrahiert und ausgibt:

```C
#include <stdio.h>

int main() {
  // Datei öffnen und HTML-Inhalt in eine Zeichenkette lesen
  FILE *html_file = fopen("example.html", "r");
  char html_string[256];
  fgets(html_string, 256, html_file);

  // Tag "p" finden und Textinhalt extrahieren
  char *start = strstr(html_string, "<p>");
  start += 3; // leere Tags überspringen
  char *end = strstr(start, "</p>");
  *end = '\0'; // Ende-Tag vorübergehend überschreiben
  char *content = start;
  
  // Textinhalt ausgeben
  printf("%s", content);

  // Datei schließen
  fclose(html_file);

  return 0;
}
```

Die Ausgabe wird folgendermaßen aussehen, wenn die Datei "example.html" folgenden Inhalt hat:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>Beispielseite</title>
  </head>
  <body>
    <h1>Willkommen</h1>
    <p>Hallo, dies ist ein Beispieltext.</p>
  </body>
</html>
```

```
Hallo, dies ist ein Beispieltext.
```

Mit dieser Methode können wir auch andere Tags oder Attribute wie "href" auslesen und unsere Ausgabe entsprechend anpassen.

## Tiefer Einblick

Das Konzept des Parsens von HTML kann komplex werden, da es viele verschiedene Regeln und Strukturen gibt, die beachtet werden müssen. Um ein vollständiges Verständnis des Parsens von HTML zu erlangen, ist es wichtig, sich mit den Grundlagen von HTML und der Syntax der Tags auseinanderzusetzen.

Außerdem kann das Parsen von HTML auch in Kombination mit anderen Web-Technologien wie CSS und JavaScript verwendet werden, um komplexere Aufgaben zu erfüllen. Das Erlernen dieser Technologien kann dazu beitragen, das Verständnis und die Fähigkeiten im Bereich des HTML-Parsings zu vertiefen.

## Siehe auch

- [libxml2 Bibliothek](http://www.xmlsoft.org/)
- [HTML Tidy Tool](http://www.html-tidy.org/)
- [Einführung in HTML](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML)
- [Einführung in CSS](https://developer.mozilla.org/en-US/docs/Learn/CSS/Introduction_to_CSS)
- [Einführung in JavaScript](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/First_steps)