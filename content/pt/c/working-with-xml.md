---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:28:02.643812-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-xml.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Trabalhar com XML em C envolve analisar, criar e manipular arquivos XML - essencialmente armazenamento de dados estruturados. Os programadores fazem isso para interagir com dados em um formato portátil e legível por humanos, frequentemente utilizado para configuração, troca de dados e mais.

## Como fazer:
Abaixo está um trecho usando a biblioteca `libxml2` para analisar um arquivo XML e capturar o elemento raiz.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *elemento_raiz = NULL;

    // Analisar o arquivo XML
    doc = xmlReadFile("exemplo.xml", NULL, 0);

    // Obter o elemento raiz
    elemento_raiz = xmlDocGetRootElement(doc);

    printf("Elemento Raiz: %s\n", elemento_raiz->name);

    // Liberar o documento
    xmlFreeDoc(doc);

    // Limpeza do analisador
    xmlCleanupParser();

    return 0;
}
```

A saída de exemplo para um XML com raiz `<data>` pode ser:
```
Elemento Raiz: data
```

## Mergulho Profundo
XML, ou Extensible Markup Language, remonta ao final dos anos 90, fornecendo uma maneira de descrever e estruturar dados. Em C, `libxml2` é a opção preferida. É robusto, embora não seja o mais fácil para iniciantes em XML. Alternativas incluem `tinyxml2`, que é mais leve e mais amigável para iniciantes. Quanto à implementação, C não possui suporte interno para XML, portanto, as bibliotecas preenchem essa lacuna. Elas variam em tamanho, velocidade, complexidade e portabilidade. A maioria oferece métodos de análise DOM e SAX: DOM carrega a coisa toda na memória, bom para documentos pequenos; SAX é orientado por eventos, lidando com elementos na hora, melhor para arquivos grandes. Ambos têm seus casos de uso e compensações.

## Veja Também
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 no GitHub](https://github.com/leethomason/tinyxml2)
- [Tutorial de XML no w3schools](https://www.w3schools.com/xml/)
- [Especificação do XML pelo W3C](https://www.w3.org/XML/)