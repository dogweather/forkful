---
title:                "Parsing de html"
html_title:           "C: Parsing de html"
simple_title:         "Parsing de html"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/parsing-html.md"
---

{{< edit_this_page >}}

##Por que

Você já se perguntou como os navegadores da Web são capazes de interpretar e exibir páginas HTML? A resposta é: através de uma técnica chamada parsing. Neste artigo, vamos explorar o que é parsing e como você pode usá-lo em suas próprias aplicações em C.

## Como fazer

Para realizar o parsing de HTML em seu código em C, você vai precisar de uma biblioteca externa chamada libxml2. Você pode instalá-la usando o gerenciador de pacotes da sua distribuição de Linux ou baixando-a diretamente do site oficial.

Uma vez que a biblioteca esteja instalada, você pode começar a utilizar suas funções para fazer o parsing de um documento HTML. Aqui está um exemplo simples que lê um arquivo HTML e imprime suas tags e conteúdo:

```C
#include <stdio.h>
#include <libxml2/libxml/HTMLparser.h>

int main() {
    // Carrega o documento HTML
    FILE* htmlFile = fopen("index.html", "r");
    htmlDocPtr doc = htmlReadFile("index.html", NULL, HTML_PARSE_NOBLANKS);
    
    // Encontra o primeiro elemento
    xmlNodePtr node = xmlDocGetRootElement(doc);
    
    // Imprime as tags e o conteúdo
    while (node != NULL) {
        printf("%s: %s\n", node->name, xmlNodeGetContent(node));
        node = node->next;
    }
    
    // Libera a memória
    xmlFreeDoc(doc);
    xmlCleanupParser();
    
    return 0;
}
```

Supondo que o arquivo "index.html" contenha o seguinte código:

```HTML
<html>
    <head>
        <title>Meu site</title>
    </head>
    <body>
        <h1>Bem-vindo ao meu site!</h1>
        <p>Aqui você encontrará conteúdo interessante sobre programação.</p>
    </body>
</html>
```

O resultado seria:

```
html:
head:
title: Meu site
body:
h1: Bem-vindo ao meu site!
p: Aqui você encontrará conteúdo interessante sobre programação.
```

Com esse exemplo simples, você já pode começar a experimentar com o parsing de HTML em suas próprias aplicações. Mas se você quiser se aprofundar mais no assunto, continue lendo para obter mais informações sobre a técnica.

## Deep Dive

O parsing de HTML é o processo de analisar um documento HTML em sua estrutura hierárquica e extrair informações relevantes, como tags e conteúdo. Isso é feito por meio de uma árvore de nós, em que cada nó representa uma parte do HTML, como um elemento ou texto.

A biblioteca libxml2 possui várias funções que facilitam o processo de parsing de HTML. Além do exemplo anterior, você pode usar outras funções para pesquisar elementos específicos, verificar a existência de atributos e muito mais.

É importante lembrar que o parsing de HTML pode ser um processo complexo, pois o documento pode conter elementos aninhados, comentários, scripts e outros elementos que podem afetar a estrutura do HTML. Portanto, é essencial entender bem a estrutura do seu documento antes de começar a implementar o parsing.

## See Also

Para mais informações sobre o parsing de HTML e outras técnicas de processamento de documentos, confira os links abaixo:

- [Documentação oficial da biblioteca libxml2](http://www.xmlsoft.org/)
- [Tutorial de parsing de HTML em C](https://michael-xiii.github.io/text/2017/11/06/HTML-Parsing-with-libxml2-in-C/)
- [Outras bibliotecas para parsing de HTML](https://www.smashingmagazine.com/2009/05/10-useful-parsing-libraries-for-php-python-and-ruby-developers/)