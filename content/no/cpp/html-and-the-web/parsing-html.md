---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:29.859211-07:00
description: "\xC5 parse HTML betyr \xE5 bryte ned HTML-innhold til noe et program\
  \ kan forst\xE5 og manipulere. Utviklere gj\xF8r dette for \xE5 trekke ut data,\
  \ manipulere innhold\u2026"
lastmod: '2024-03-13T22:44:41.097389-06:00'
model: gpt-4-0125-preview
summary: "\xC5 parse HTML betyr \xE5 bryte ned HTML-innhold til noe et program kan\
  \ forst\xE5 og manipulere. Utviklere gj\xF8r dette for \xE5 trekke ut data, manipulere\
  \ innhold\u2026"
title: Analysering av HTML
weight: 43
---

## Hva & Hvorfor?
Å parse HTML betyr å bryte ned HTML-innhold til noe et program kan forstå og manipulere. Utviklere gjør dette for å trekke ut data, manipulere innhold eller integrere web scraping i applikasjonene sine.

## Hvordan:
C++ kommer ikke med innebygd funksjonalitet for parsing av HTML. Du vil ofte bruke et bibliotek som Gumbo-parser fra Google, eller noe lignende. Her er et raskt eksempel ved bruk av Gumbo-parser:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        hvis (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* barn = &node->v.element.children;
    for (unsigned int i = 0; i < barn->length; ++i) {
        search_for_links(static_cast<GumboNode*>(barn->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Lenke</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Eksempel på utdata:
```
https://example.com
```

## Dypere dykk
Det har ikke alltid vært enkelt å parse HTML i C++. Historisk sett ville programmerere bruke regex eller håndskrevne parser, begge deler er feilutsatte og omstendelige. I dag tar robuste biblioteker som Gumbo-parser seg av kompleksiteten med parsing, noe som gjør det enklere og mer pålitelig.

Alternativer inkluderer Tidy, MyHTML, eller til og med å integrere C++ med Pythons BeautifulSoup via C++ `system` funksjonen eller innebygde tolkere.

Når det gjelder implementasjon, konverterer disse bibliotekene HTML til et Document Object Model (DOM) tre. Å traversere og manipulere DOM gjør det mulig for brukere å trekke ut og arbeide med data som demonstrert i Hvordan-delen.

## Se også
- [Gumbo-parser GitHub-repositoriet](https://github.com/google/gumbo-parser)
- [Liste over HTML-parsingsbiblioteker](https://en.cppreference.com/w/c/experimental/dynamic)
- [C++ og Python-interoperabilitet](https://docs.python.org/3/extending/embedding.html)
