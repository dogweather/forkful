---
title:                "HTML:n jäsennys"
aliases:
- /fi/cpp/parsing-html.md
date:                  2024-02-03T19:11:47.985829-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTML:n jäsentäminen tarkoittaa HTML-sisällön hajottamista joksikin, jonka ohjelma voi ymmärtää ja käsitellä. Ohjelmoijat tekevät tämän, jotta voivat poimia tietoja, muokata sisältöä tai integroida verkkoskrapausta sovelluksiinsa.

## Kuinka:
C++ ei tule sisäänrakennettujen HTML-jäsentämismahdollisuuksien kanssa. Usein käytät kirjastoa, kuten Google:n Gumbo-parser tai vastaavaa. Tässä on nopea esimerkki käyttäen Gumbo-parseria:

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
```

Esimerkkituloste:
```
https://example.com
```

## Syväsukellus
HTML:n jäsentäminen ei ole aina ollut suoraviivaista C++:ssa. Historiallisesti ohjelmoijat käyttivät regexiä tai käsinkirjoitettuja jäsentäjiä, jotka molemmat ovat virhealttiita ja kömpelöitä. Nykyään, vankat kirjastot, kuten Gumbo-parser, käsittelevät jäsentämisen monimutkaisuuksia, mikä tekee siitä helpompaa ja luotettavampaa.

Vaihtoehtoja sisältävät Tidy, MyHTML, tai jopa C++:n integroiminen Pythonin BeautifulSoupin kanssa käyttäen C++ `system` toimintoa tai upotettuja tulkkeja.

Toteutuksen kannalta, nämä kirjastot muuttavat HTML:n Dokumenttiobjektimalliksi (DOM) puuksi. DOM-puun läpikäyminen ja manipulointi mahdollistaa käyttäjille tietojen poiminnan ja työstämisen, kuten Kuinka-osiossa on esitetty.

## Katso Myös
- [Gumbo-parser GitHub varasto](https://github.com/google/gumbo-parser)
- [Lista HTML-jäsentämiskirjastoista](https://en.cppreference.com/w/c/experimental/dynamic)
- [C++ ja Python yhteentoimivuus](https://docs.python.org/3/extending/embedding.html)
