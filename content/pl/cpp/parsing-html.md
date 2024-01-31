---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:30:37.482508-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie HTML to proces wydobywania danych ze struktury dokumentu HTML. Programiści robią to, by manipulować treścią, wydobywać informacje, czy integracji z innymi aplikacjami.

## Jak to zrobić:
Aby sparsować HTML w C++, możesz skorzystać z biblioteki `Gumbo-parser` od Google. Poniżej znajdziesz prosty przykład użycia:

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
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

Wyjście z tego programu:
```
https://example.com
```

## Deep Dive:
Parsowanie HTML powstało wraz z potrzebą analizy i interakcji z siecią WWW. Wczesne metody opierały się na prostych sztuczkach tekstowych i wyrażeniach regularnych, ale były podatne na błędy. Obecnie mamy solidne parsery, które rozumieją skomplikowaną strukturę HTML.

Alternatywy dla `Gumbo-parser` to na przykład `htmlcxx`, `MyHTML` czy `libxml2` z modułem HTML. Wybór biblioteki zależy od wymagań projektowych - wydajności, licencji, łatwości użycia.

Implementacja parsera HTML zawiera masę detali – obsługę błędów w kodzie HTML, rozróżnianie elementów DOM, czy zarządzanie pamięcią. Tworzenie własnego parsera od zera jest wyzwaniem, więc częściej wybierane są gotowe rozwiązania.

## Zobacz także:
- Dokumentacja Gumbo-parser: https://github.com/google/gumbo-parser
- Strona projektu htmlcxx: http://htmlcxx.sourceforge.net/
- Repozytorium MyHTML: https://github.com/lexborisov/myhtml
- Informacje o libxml2: http://xmlsoft.org/html/libxml-HTMLparser.html
