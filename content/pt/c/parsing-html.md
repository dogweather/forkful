---
title:                "Analisando HTML"
html_title:           "C: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/parsing-html.md"
---

{{< edit_this_page >}}

# Parseamento HTML em C: Simplificando o Código

## O Que & Por quê?

Parsear HTML é explorar e sintetizar as tags e conteúdo do código HTML. Programadores fazem isso para extrair informações específicas de uma página da web ou manipular sua estrutura.

## Como fazer:

Aqui está um exemplo simples de como você pode parsear HTML em C usando a biblioteca Gumbo:

```C
#include <stdio.h>
#include <gumbo.h>

static void buscar_links (GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }

    GumboAttribute* href;

    if (node->v.element.tag == GUMBO_TAG_A && (href = gumbo_get_attribute (&node->v.element.attributes, "href"))) {
        printf ("%s\n", href->value);
    }

    GumboVector* filhos = &node->v.element.children;

    for (unsigned int i = 0; i < filhos->length; ++i) {
        buscar_links ((GumboNode*) filhos->data[i]);
    }
}

int main () {
    GumboOutput* output = gumbo_parse ("<h1>Olá, mundo!</h1><a href='http://exemplo.com'>Exemplo</a>");

    buscar_links (output->root);

    gumbo_destroy_output (&kGumboDefaultOptions, output);
    return 0;
}
```

Ao executar o código acima, você deve ver a seguinte saída:

```C
http://exemplo.com
```

## Imersão completa

O parseamento HTML data dos primeiros dias da web. Anteriormente, tinha que ser feito manualmente, mas agora existem bibliotecas como a Gumbo, que simplificam este processo.

Uma alternativa à Gumbo é a libxml2, que também pode parsear HTML, mas é um pouco mais complexa de usar.

Por trás dos panos, o parseamento HTML envolve a leitura do HTML como uma string, a identificação das tags e a estruturação dessas tags em uma árvore ou outro formato manipulável.

## Veja também

Gumbo - https://github.com/google/gumbo-parser

Libxml2 - http://xmlsoft.org/

Tutorial de Parseamento HTML em C - https://www.htmlgoodies.com/tutorials/getting_started/article.php/3479511/how-to-parse-html-pages-with-c.htm