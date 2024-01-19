---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTML-Parsing ist im Grunde das Extrahieren nützlicher Daten aus einem HTML-Dokument. Programmierer machen das, um Informationen aus einer bestimmten Webseite zu sammeln oder die Struktur der Webseite zu analysieren.

## So geht's:

Wir können die Bibliothek Beautiful Soup in Kombination mit C++ verwenden, um HTML zu parsen. Hier ein einfaches Beispiel:

```C++
#include <iostream>
#include <soup/parser.hpp>

int main()
{
    std::string html = "<html><body><h1>Willkommen zum HTML Parsing!</h1></body></html>";
    Soup::Parser parser;
    Soup::Tag h1 = parser.parse(html);

    std::cout << h1.name() << ": " << h1.text() << std::endl;
    return 0;
}
```
Probieren Sie es aus! Der Ausgabestring sollte `h1: Willkommen zum HTML Parsing!` sein.

## Tiefere Einblicke

Der Prozess des HTML-Parsing ist nicht neu. Er hat seine Wurzeln in den Anfängen des Webs, als Webcrawler entwickelt wurden, um Webseiten zu erfassen. Heute gibt es viele Alternativen wie das Parsing mit Python und seine Bibliotheken wie lxml und html.parser.

In C++ würde man in den meisten Fällen Bibliotheken wie Gumbo oder BeautifulSoup verwenden. Dabei wird zunächst das HTML-Dokument in einen DOM (Document Object Model) umgewandelt, welcher anschließend analysiert wird. Wichtig dabei ist es, fehlerfrei geschriebenes HTML zu haben, da sonst der Prozess fehlschlagen kann.

## Siehe auch

1. "HTML Parsing mit Python": %[http://www.xyz.de/html-parse-python]
2. "Gumbo, eine C++-Bibliothek zum Parsen von HTML": %[http://www.xyz.de/gumbo]
3. "Fehlervermeidung beim Schreiben von HTML": %[http://www.xyz.de/fehlervermeidung]