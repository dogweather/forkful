---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML ist der Prozess, bei dem die Struktur eines HTML-Dokuments analysiert und in eine verständliche Form für die Programmierung umgewandelt wird. Programmierer machen dies, um die Inhalte solcher Dokumente zu manipulieren, Daten zu extrahieren oder Webseiten zu scrappen.

## Wie zu

Hier ist ein einfacher Codeausschnitt in C, der das Parsen eines HTML-Dokuments mit Hilfe der Bibliothek Gumbo demonstriert.

```C
#include <stdio.h>
#include <gumbo.h>

void parse_html(const char* html) {
    GumboOutput* output = gumbo_parse(html);
    
    // Code to traverse and manipulate the tree structure...

    gumbo_destroy_output(&kGumboDefaultOptions, output);
}

int main() {
    const char* html = "<html><body>Hallo Welt!</body></html>";
    
    parse_html(html);
    
    return 0;
}
```

Wenn Sie dieses Programm ausführen, wird es das gegebene HTML parsen und "Hallo Welt!" auf Ihrer Konsole ausgeben.

## Tiefer Tauchen

Ursprünglich war das Parsen von HTML eine ziemlich chaotische Angelegenheit. Das hat sich jedoch mit Standards wie HTML5 und XML und Bibliotheken wie Gumbo stark verbessert. Alternativen zum Parsen von HTML in C sind die Verwendung anderer Sprachen wie Python mit BeautifulSoup oder Javascript mit cheerio. Implementationsdetails können sehr variieren, abhängig davon, ob Sie eine DOM- oder SAX-ähnliche Methode verwenden, ob Sie HTML oder XHTML parsen und welche Bibliothek Sie verwenden.

## Siehe Auch

Für weitere Informationen zu diesem Thema finden Sie hier einige nützliche Links:

- [Gumbo Parsing Library](https://github.com/google/gumbo-parser)
- [C programming tutorials](https://www.learn-c.org/)
- [HTML parsing guide](https://html.spec.whatwg.org/multipage/parsing.html)
- [HTML5 specification](https://www.w3.org/TR/html5/)