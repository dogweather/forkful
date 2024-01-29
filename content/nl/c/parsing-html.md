---
title:                "HTML Parsen"
date:                  2024-01-28T22:04:08.495825-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

HTML parsen betekent het lezen en begrijpen van de structuur van HTML-documenten door een programma. Programmeurs doen dit om content te manipuleren, extraheren of controleren, vaak tijdens het scrapen van websites of het verwerken van webgegevens.

## Hoe:

Oké, laten we naar de code gaan. C heeft geen ingebouwde ondersteuning voor het parsen van HTML, dus we gebruiken een bibliotheek genaamd Gumbo, dit is een zuivere C HTML5-parser. Hier is een snel voorbeeld:

```C
#include <stdio.h>
#include <gumbo.h>

void zoek_naar_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
       (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        printf("Link gevonden: %s\n", href->value);
    }
    GumboVector* kinderen = &node->v.element.children;
    for (unsigned int i = 0; i < kinderen->length; ++i) {
        zoek_naar_links(kinderen->data[i]);
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Voorbeeld</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    zoek_naar_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Voorbeelduitvoer:

```
Link gevonden: https://example.com
```

Dit voorbeeld zoekt naar 'a' tags en print de href-attributen uit. Vergeet niet te linken tegen gumbo (`gcc -o voorbeeld voorbeeld.c -lgumbo`) en eerst de bibliotheek te installeren.

## Diepgaande Duik

Het verhaal van HTML-parsing in C is een beetje ruw. Er is geen one-size-fits-all oplossing omdat HTML complex en meestal niet zo consistent is. Gumbo, die we hebben gebruikt, werd ontwikkeld door Google als onderdeel van hun open-sourceprojecten. Het is ontworpen om de rommeligheid van webpagina's in de echte wereld te tolereren.

Alternatieven zijn onder meer libxml2 met een HTML-parsermodus, hoewel het historisch meer op XML-parsing is gericht. Een ander is htmlcxx, wat eigenlijk C++ is, maar laten we niet afdwalen.

Wat betreft prestaties, kunnen C-parsers razendsnel zijn maar bieden normaal gesproken niet het gebruiksgemak dat Python-bibliotheken doen. Als je C gebruikt voor HTML-parsing, ben je waarschijnlijk op zoek naar prestaties, of je integreert het in een bestaande C-codebasis. Het kan lastig zijn, aangezien de meeste C-bibliotheken low-level en meer hands-on zijn dan Python of JavaScript-parsers.

## Zie Ook

- Gumbo Parser: [https://github.com/google/gumbo-parser](https://github.com/google/gumbo-parser)
- libxml2 HTML parser: [http://xmlsoft.org/html/libxml-HTMLparser.html](http://xmlsoft.org/html/libxml-HTMLparser.html)
- htmlcxx: [http://htmlcxx.sourceforge.net/](http://htmlcxx.sourceforge.net/) 
- Voor een zachte start, overweeg een tutorial over webscrapen met Python met behulp van Beautiful Soup of Python's `html.parser` als een eenvoudigere introductie tot het onderwerp.
