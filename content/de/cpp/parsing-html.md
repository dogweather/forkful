---
title:                "HTML parsen"
date:                  2024-01-20T15:30:20.790421-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTML-Parsing ist das Auslesen und Verarbeiten von HTML-Code durch ein Programm, um dessen Struktur und Inhalte zu verstehen. Programmierer machen das, um Webinhalte zu manipulieren, Daten zu extrahieren oder Dokumente für verschiedene Anwendungen dynamisch zu generieren.

## Wie geht das?
```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
        (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        std::cout << href->value << std::endl;
    }
    
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='http://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```
Ausgabe:
```
http://example.com
```

## Deep Dive
Parsing HTML in C++ ist ein nicht-triviales Unterfangen. Die Standardsprache verfügt nicht über eingebaute Libraries dafür, doch es gibt Drittanbieter-Lösungen wie Gumbo - ein von Google entwickelter HTML5-Parser. Alternativ könnten RegExp oder String-Manipulation verwendet werden, was jedoch nicht robust und fehleranfällig ist.

Der Gumbo-Parser ist in reinem C geschrieben und für C++-Projekte geeignet. Er bietet eine saubere API, um HTML-Dokumente zu durchsuchen, was das Extrahieren spezifischer Daten erleichtert. Das Verarbeiten von HTML mit Gumbo ist sicherer als regelbasierte Ansätze und hält sich an die HTML5-Spezifikation.

Im Laufe der Zeit haben sich auch leistungsfähige Libraries wie BeautifulSoup für Python etabliert. C++-Entwickler könnten also überlegen, ob für bestimmte Aufgaben ein Sprachwechsel sinnvoll ist, oder ob die Performance und Komplexität von C++ erforderlich sind.

## See Also
- Gumbo Parser: https://github.com/google/gumbo-parser
- HTML5-Spezifikation: https://html.spec.whatwg.org/multipage/
- C++ Referenz: https://en.cppreference.com/
- BeautifulSoup Dokumentation (für Python-Entwickler): https://www.crummy.com/software/BeautifulSoup/bs4/doc/
