---
title:                "Análise de html"
html_title:           "C++: Análise de html"
simple_title:         "Análise de html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## O que e por que?

Analisar HTML é o processo de ler e interpretar o código de uma página da web, permitindo que os programadores possam acessar e manipular informações específicas dentro de uma página. Os programadores muitas vezes fazem isso para extrair dados de um site ou para criar aplicativos que interagem com sites existentes.

## Como fazer:

Existem várias maneiras de realizar a análise de HTML em C++, mas uma das formas mais populares é usando uma biblioteca externa chamada "libxml2". Aqui está um exemplo de como usar essa biblioteca para obter o título de uma página da web:

```
#include <libxml/HTMLparser.h> // inclua a biblioteca

int main() {
    const char * htmlInput; // entrada HTML
    htmlDocPtr doc = htmlReadMemory(htmlInput, strlen(htmlInput), NULL, NULL, HTML_PARSE_RECOVER); // cria um documento a partir da entrada HTML
    xmlNode * root = xmlDocGetRootElement(doc); // obtém o elemento raiz do documento
    xmlNode * head = root->children; // obtém o elemento "head" dentro do elemento raiz
    xmlNode * title = head->children; // obtém o elemento "title" dentro do elemento "head"
    xmlChar * titleContent = xmlNodeGetContent(title); // obtém o conteúdo do elemento "title"
    printf("%s\n", titleContent); // imprime o título
    xmlFreeDoc(doc); // libera o documento
    xmlCleanupParser(); // limpa o parser
    return 0;
}
```

A saída desse código seria o título da página da web.

## Mergulho profundo:

A análise de HTML é uma prática antiga na programação, que remonta aos primeiros dias da internet. Existem várias bibliotecas disponíveis para análise de código HTML em C++, como a "libxml2" mencionada anteriormente e a "libhtmlcxx". Além disso, muitas linguagens de programação modernas, como Python e JavaScript, têm recursos embutidos para análise de HTML, tornando essa tarefa ainda mais fácil.

## Veja também:

Para saber mais sobre análise de HTML em C++, confira os seguintes recursos:

- [Site oficial da biblioteca libxml2](http://www.xmlsoft.org/)
- [Tutorial de análise de HTML usando a biblioteca libxml2](https://www.xmlsoft.org/html/libxml-HTMLparser.html)
- [Documentação da biblioteca libhtmlcxx](http://www.mbayer.de/htmlcxx/)
- [Tutoriais de análise de HTML em outras linguagens de programação](https://realpython.com/html-and-python/), [JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)