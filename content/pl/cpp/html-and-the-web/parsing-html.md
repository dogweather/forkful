---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:39.403985-07:00
description: "Jak to zrobi\u0107: C++ nie posiada wbudowanych mo\u017Cliwo\u015Bci\
  \ parsowania HTML. Cz\u0119sto u\u017Cywa si\u0119 do tego biblioteki, takiej jak\
  \ Gumbo-parser od Google'a, lub\u2026"
lastmod: '2024-03-13T22:44:35.710650-06:00'
model: gpt-4-0125-preview
summary: "C++ nie posiada wbudowanych mo\u017Cliwo\u015Bci parsowania HTML."
title: "Analiza sk\u0142adniowa HTML"
weight: 43
---

## Jak to zrobić:
C++ nie posiada wbudowanych możliwości parsowania HTML. Często używa się do tego biblioteki, takiej jak Gumbo-parser od Google'a, lub czegoś podobnego. Oto krótki przykład użycia Gumbo-parser:

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
    GumboVector* dzieci = &node->v.element.children;
    for (unsigned int i = 0; i < dzieci->length; ++i) {
        search_for_links(static_cast<GumboNode*>(dzieci->data[i]));
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

Przykładowe wyjście:
```
https://example.com
```

## Dogłębna analiza
Parsowanie HTML nigdy nie było prostym zadaniem w C++. Historycznie, programiści używali wyrażeń regularnych lub pisanych ręcznie parserów, obie metody są podatne na błędy i uciążliwe. Obecnie, solidne biblioteki takie jak Gumbo-parser zajmują się zawiłościami parsowania, czyniąc je łatwiejszym i bardziej niezawodnym.

Do alternatyw należą Tidy, MyHTML, czy nawet integracja C++ z Pythonowym BeautifulSoup za pośrednictwem funkcji `system` w C++ lub wbudowanych interpreterów.

Pod względem implementacji, te biblioteki konwertują HTML na drzewo modelu obiektowego dokumentu (DOM). Przechodzenie i manipulowanie DOM pozwala użytkownikom na wyodrębnianie i pracę z danymi, jak pokazano w sekcji Jak to zrobić.

## Zobacz też
- [Repozytorium Gumbo-parser na GitHubie](https://github.com/google/gumbo-parser)
- [Lista bibliotek do parsowania HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Współpraca C++ i Pythona](https://docs.python.org/3/extending/embedding.html)
