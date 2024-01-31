---
title:                "Análise de HTML"
date:                  2024-01-20T15:30:52.091200-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"

category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Analisar HTML é o processo de transformação de texto HTML bruto em algo compreensível e manipulável por programas. Programadores fazem isso para extrair informações, automatizar interações com páginas web ou para testar conteúdos de sites.

## Como Fazer:
O exemplo a seguir usa a biblioteca `Gumbo-parser` para analisar HTML em C++. Primeiro, instale a biblioteca e inclua-a no seu arquivo.

```C++
#include <iostream>
#include <gumbo.h>

static void buscar_links(const GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }

    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
        (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        std::cout << href->value << std::endl;
    }

    const GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        buscar_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    
    buscar_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

Saída esperada:
```
https://example.com
```

## Análise Profunda:
Antigamente, a análise de HTML em C++ era complicada e propensa a erros, mas hoje, com a ajuda de bibliotecas como a `Gumbo-parser`, tornou-se muito mais simples. Alternativas incluem `libxml2` e `Htmlcxx`. Cada biblioteca tem suas próprias vantagens, dependendo da complexidade do HTML e do tipo de informação que você quer extrair. A implementação detalhada acima é somente um basicão - a `Gumbo-parser` suporta muito mais detalhes sofisticados, como manipulação de nós, atributos e muito mais.

## Veja Também:
- [Documentação da Gumbo-parser](https://github.com/google/gumbo-parser)
- [Tutorial de libxml2](http://xmlsoft.org/html/libxml-HTMLparser.html)
- [Referência de Htmlcxx](http://htmlcxx.sourceforge.net/)
- [Tutorial de BeautifulSoup para Python (se quiser experimentar algo em alto nível)](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
