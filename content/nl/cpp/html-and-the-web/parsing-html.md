---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:30.764590-07:00
description: "HTML parsen betekent het afbreken van HTML-inhoud tot iets dat een programma\
  \ kan begrijpen en manipuleren. Programmeurs doen dit om gegevens te\u2026"
lastmod: '2024-03-13T22:44:51.110502-06:00'
model: gpt-4-0125-preview
summary: HTML parsen betekent het afbreken van HTML-inhoud tot iets dat een programma
  kan begrijpen en manipuleren.
title: HTML Parsen
weight: 43
---

## Hoe te:
C++ komt niet met ingebouwde HTML-parsingmogelijkheden. Je zal vaak een bibliotheek zoals Gumbo-parser van Google gebruiken, of iets vergelijkbaars. Hier is een snel voorbeeld met Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void zoek_naar_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* kinderen = &node->v.element.children;
    for (unsigned int i = 0; i < kinderen->length; ++i) {
        zoek_naar_links(static_cast<GumboNode*>(kinderen->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    zoek_naar_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Voorbeelduitvoer:
```
https://example.com
```

## Diepgaande Duik
HTML parsen is niet altijd eenvoudig geweest in C++. Historisch gezien, zouden programmeurs regex of handgeschreven parsers gebruiken, die beide foutgevoelig en omslachtig zijn. Tegenwoordig handelen robuuste bibliotheken zoals Gumbo-parser de complexiteiten van het parsen af, waardoor het gemakkelijker en betrouwbaarder wordt.

Alternatieven zijn onder andere Tidy, MyHTML, of zelfs het integreren van C++ met Python's BeautifulSoup via de C++ `system` -functie of ingebedde interpreters.

Implementatiegewijs converteren deze bibliotheken HTML naar een Document Object Model (DOM) boom. Het traverseren en manipuleren van de DOM stelt gebruikers in staat om gegevens te extraheren en ermee te werken zoals gedemonstreerd in de sectie Hoe te.

## Zie Ook
- [Gumbo-parser GitHub-repository](https://github.com/google/gumbo-parser)
- [Lijst van HTML-parsingbibliotheken](https://nl.cppreference.com/w/c/experimental/dynamic)
- [C++ en Python interoperabiliteit](https://docs.python.org/3/extending/embedding.html)
