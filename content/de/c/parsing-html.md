---
title:                "HTML parsen"
date:                  2024-01-20T15:30:22.426444-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/parsing-html.md"
---

{{< edit_this_page >}}

# HTML-Parsing: Was & Warum?

HTML-Parsing ermöglicht es Programmen, die Struktur von Webseiten zu verstehen und Inhalte gezielt auszulesen. Entwickler machen das, um Daten zu sammeln, Web-Scraping zu betreiben oder Inhalte automatisiert zu verarbeiten.

# So geht's:

Librarys wie `libxml2` bieten in C Tools fürs HTML-Parsing. Hier ein simples Beispiel:

```C
#include <stdio.h>
#include <libxml/HTMLparser.h>

int main() {
    htmlDocPtr doc;
    htmlNodePtr node;

    // HTML-String, kann auch aus einer Datei oder dem Internet stammen
    char *html = "<html><body><p>Hello, World!</p></body></html>";

    // HTML-Parser initialisieren und Dokument parsen
    doc = htmlReadDoc((xmlChar*)html, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

    // root element holen
    node = xmlDocGetRootElement(doc);

    // einfach über die Knoten iterieren und Namen ausgeben
    for (node = node->children; node; node = node->next) {
        printf("Element Name: %s\n", (char *) node->name);
    }

    // Speicher freigeben
    xmlFreeDoc(doc);

    return 0;
}
```

Ausgabe könnte so aussehen:

```
Element Name: body
Element Name: p
```

# Hinter den Kulissen:

In der Vergangenheit wurde HTML oft mit regulären Ausdrücken (regex) geparsed, was fehleranfällig ist. HTML ist keine reguläre Sprache und kann mit regex nicht korrekt geparst werden. Deshalb greifen Entwickler auf spezialisierte Parser wie `libxml2` zurück.

Alternativen zu `libxml2` sind `Gumbo` von Google oder `htmlcxx` für C++. Diese nutzen unterschiedliche Ansätze und APIs, doch das Ziel bleibt dasselbe: verlässliches Parsing von HTML.

Beim Parsing intern wird der HTML-Text in einen DOM (Document Object Model) überführt. Das ermöglicht es, auf einzelne Teile der Struktur zuzugreifen, als wären sie Teile eines baumartig aufgebauten Graphen.

# Siehe auch:

- Die `libxml2`-Dokumentation: http://xmlsoft.org/html/libxml-HTMLparser.html
- `Gumbo` Parser: https://github.com/google/gumbo-parser
- `htmlcxx` HTML/CSS-Parser in C++: http://htmlcxx.sourceforge.net/
- W3C zu HTML und DOM: https://www.w3.org/DOM/
