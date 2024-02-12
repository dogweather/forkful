---
title:                "Analisando HTML"
aliases: - /pt/cpp/parsing-html.md
date:                  2024-02-03T19:11:35.767137-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Analisar HTML significa decompor o conteúdo HTML em algo que um programa possa entender e manipular. Os programadores fazem isso para extrair dados, manipular conteúdo ou integrar a raspagem da web em suas aplicações.

## Como fazer:
C++ não vem com capacidades integradas de análise de HTML. Você frequentemente usará uma biblioteca como Gumbo-parser do Google, ou algo semelhante. Aqui está um exemplo rápido usando Gumbo-parser:

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

Saída de exemplo:
```
https://example.com
```

## Mergulhando Fundo
Analisar HTML nem sempre foi algo direto em C++. Historicamente, os programadores usariam regex ou analisadores escritos manualmente, ambos propensos a erros e pesados. Atualmente, bibliotecas robustas como Gumbo-parser lidam com as complexidades da análise, tornando-a mais fácil e mais confiável.

Alternativas incluem Tidy, MyHTML, ou até mesmo integrar C++ com BeautifulSoup do Python através da função `system` do C++ ou interpretadores embutidos.

Em termos de implementação, essas bibliotecas convertem HTML para uma árvore de Modelo de Objeto de Documento (DOM). Percorrer e manipular o DOM permite aos usuários extrair e trabalhar com dados conforme demonstrado na seção Como fazer.

## Veja Também
- [Repositório do GitHub do Gumbo-parser](https://github.com/google/gumbo-parser)
- [Lista de bibliotecas de análise de HTML](https://en.cppreference.com/w/c/experimental/dynamic)
- [Interoperabilidade entre C++ e Python](https://docs.python.org/3/extending/embedding.html)
