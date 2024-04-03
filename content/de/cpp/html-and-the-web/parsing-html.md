---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:24.329034-07:00
description: "HTML zu parsen bedeutet, HTML-Inhalte so aufzubrechen, dass ein Programm\
  \ sie verstehen und manipulieren kann. Programmierer tun dies, um Daten zu\u2026"
lastmod: '2024-03-13T22:44:54.182620-06:00'
model: gpt-4-0125-preview
summary: HTML zu parsen bedeutet, HTML-Inhalte so aufzubrechen, dass ein Programm
  sie verstehen und manipulieren kann.
title: HTML parsen
weight: 43
---

## Was & Warum?
HTML zu parsen bedeutet, HTML-Inhalte so aufzubrechen, dass ein Programm sie verstehen und manipulieren kann. Programmierer tun dies, um Daten zu extrahieren, Inhalte zu manipulieren oder Web-Scraping in ihre Anwendungen zu integrieren.

## Wie geht das:
C++ verfügt nicht über integrierte Fähigkeiten zum Parsen von HTML. Sie werden oft eine Bibliothek wie Gumbo-parser von Google oder Ähnliches verwenden. Hier ist ein kurzes Beispiel mit Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* Kinder = &node->v.element.children;
    for (unsigned int i = 0; i < Kinder->length; ++i) {
        search_for_links(static_cast<GumboNode*>(Kinder->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Beispielausgabe:
```
https://example.com
```

## Tiefergehend
HTML in C++ zu parsen war nicht immer unkompliziert. Historisch gesehen würden Programmierer Regex oder selbstgeschriebene Parser verwenden, die beide fehleranfällig und mühsam sind. Heutzutage bewältigen robuste Bibliotheken wie Gumbo-parser die Komplexitäten des Parsens und machen es einfacher und zuverlässiger.

Alternativen beinhalten Tidy, MyHTML oder sogar die Integration von C++ mit Pythons BeautifulSoup über die C++ `system` Funktion oder eingebettete Interpreter.

Implementierungstechnisch konvertieren diese Bibliotheken HTML in einen Document Object Model (DOM) Baum. Das Durchlaufen und Manipulieren des DOM ermöglicht es Benutzern, Daten zu extrahieren und zu bearbeiten, wie im Wie geht das-Abschnitt demonstriert.

## Siehe auch
- [Gumbo-parser GitHub-Repository](https://github.com/google/gumbo-parser)
- [Liste von HTML-Parsing-Bibliotheken](https://en.cppreference.com/w/c/experimental/dynamic)
- [Interoperabilität zwischen C++ und Python](https://docs.python.org/3/extending/embedding.html)
