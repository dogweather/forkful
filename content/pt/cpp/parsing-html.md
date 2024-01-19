---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Por quê?

Analisar HTML (parser HTML) é o processo de examinar e interpretar um código HTML para transformá-lo em uma estrutura de dados compreensível. Os programadores fazem isso para extrair informações, manipular e interagir com elementos web em um nível mais profundo.

## Como fazer:

Aqui temos um exemplo básico de análise de HTML utilizando a biblioteca Gumbo do Google para C++. Este pequeno trecho de código vai analisar um documento HTML e imprimir o título do documento na saída padrão.

```C++
#include <iostream>
#include "gumbo.h"

int main() {
    const std::string html = "<!DOCTYPE html><html><head><title>Olá mundo!</title></head><body><p>Exemplo de parser HTML.</p></body></html>";

    GumboOutput* output = gumbo_parse(html.c_str());

    GumboNode* node = output->root;

    if (node->v.element.children.length >= 2) {
        GumboNode* title = static_cast<GumboNode*>(node->v.element.children.data[1]);
        if (title->type == GUMBO_NODE_ELEMENT && title->v.element.tag == GUMBO_TAG_TITLE) {
            std::cout << "Titulo: " << std::string(static_cast<GumboText*>(title->v.element.children.data[0])->text) << std::endl;
        }
    }

    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```
A saída desse código será:
```
Titulo: Olá mundo!
```

## Aprofundamento

A necessidade de analisar HTML remonta aos primeiros dias da web. Originalmente, o HTML era uma linguagem de marcação simples que permitia aos desenvolvedores criar páginas da web estáticas. Com o tempo, no entanto, tornou-se necessário interagir com essas páginas em um nível mais profundo, e assim a análise de HTML nasceu.

Há várias alternativas para a análise de HTML em C++. Além da biblioteca Gumbo, temos também o MyHTML, htmlcxx, e muitos outros. A decisão de usar uma biblioteca ou outra depende das necessidades do seu projeto. 

A implementação da análise de HTML é uma tarefa complexa, pois precisa lidar com a natureza muitas vezes mal definida e inconsistente do HTML na web. No entanto, as bibliotecas modernas, como o Gumbo, aproveitam algoritmos sofisticados e uma profunda compreensão da especificação HTML para superar esses obstáculos.

## Veja Também

Para mais informações sobre a biblioteca Gumbo, você pode acessar a documentação oficial do Google nesta [link](https://github.com/google/gumbo-parser).

Para aprender mais sobre análise HTML em geral, este [artigo](https://www.w3.org/TR/html51/syntax.html#parsing) do W3C é um ótimo recurso. 

Além disso, aqui está uma [comparação](https://kripken.github.io/compare-parser/) entre várias bibliotecas de análise HTML em C++.