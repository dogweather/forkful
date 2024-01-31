---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:31:04.547291-07:00
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
HTML:n jäsentäminen on prosessi, jossa HTML-kieltä käsitellään tunnistamalla sen rakenteet ja sisältö. Ohjelmoijat jäsentävät HTML:ää datan kaivamiseen, sisällön skrapaamiseen tai web-sovellusten lukuisten toimintojen automatisointiin.

## How to: - Kuinka:
```C++
#include <iostream>
#include <gumbo.h> // Käytetään Gumbo HTML parseria.

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
    const char* html = "<html><body><a href='https://example.com'>Linkki</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}

// Tulostus:
// https://example.com
```
Koodissa käytetään Gumbo-parseria linkkien etsimiseen HTML:stä.

## Deep Dive - Syväsukellus:
HTML-jäsentäminen on ollut tarpeellista webin alkuajoista lähtien. Selaimet jäsentävät HTML:ää näyttääkseen sivuja oikein, mutta backend-kehityksessä tarve syntyy datan keruusta. Vaihtoehtoina ovat eri kirjastot kuten Gumbo tai libxml2 C++:ssa.

Libxml2 on vanhempi ja laajempi kirjasto, kun taas Gumbo on Googlen kehittämä moderni ja helppokäyttöinen parseri. Molemmat ovat avoimen lähdekoodin ja niillä on omat heikkoutensa ja vahvuutensa riippuen käytöstä.

Gumbon implementaatio perustuu HTML5-spesifikaatioon, mikä helpottaa nykyaikaisten web-sivujen käsittelyä. Jäsentäjän valintaan voi vaikuttaa sen tuki erilaisille HTML-versioille, tarve DOM-puun manipulointiin tai suorituskykyvaatimukset.

## See Also - Katso Myös:
- Gumbo Parser: https://github.com/google/gumbo-parser
- Libxml2: http://xmlsoft.org/html/libxml-HTMLparser.html
- HTML5 Spec: https://html.spec.whatwg.org/

Tämä artikkeli antaa pohjatiedot ja esimerkin siitä, kuinka aloittaa HTML:n jäsentäminen C++:lla. Linkit toisille sivustoille tarjoavat lisätietoa ja syventävät ymmärrystä aiheesta.
