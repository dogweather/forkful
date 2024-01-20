---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/parsing-html.md"
---

{{< edit_this_page >}}

## O que & Por quê?

A análise de HTML (parsing HTML) é o processo de extrair informações específicas de documentos HTML. Programadores fazem isso para obter dados estruturados a partir de páginas web.

## Como Fazer:

Usaremos `html-xml-utils`, uma coleção de utilitários para manipulação de documentos HTML. Primeiro instale utilizando o comando:

```Bash
sudo apt-get install html-xml-utils
```

Agora vamos fazer parse de um HTML simples:

```Bash
echo '<p>Olá, mundo!</p>' | hxnormalize -x
```

A saída será:

```Bash
<P>
  Olá, mundo!
</P>
```

Entre as tags HTML, você encontrará o texto "Olá, mundo!".

## Mergulho Profundo:

1. **Contexto Histórico**: O parsing de HTML começou em meados dos anos 90, quando a web estava começando a prosperar. Programadores precisavam de uma maneira de extrair informações úteis das páginas da web.
   
2. **Alternativas**: Existem outras ferramentas e idiomas para fazer parsing de HTML, entre eles BeautifulSoup (em Python), Nokogiri (em Ruby), Jsoup (em Java). Cada um tem sua própria vantagem, dependendo do seu use case e do ambiente de programação.
   
3. **Detalhes de Implementação**: No Bash, o parsing de HTML faz uso de utilitários como `html-xml-utils`, que fornecem funções para manipular HTML. No entanto, parsing de ponta a ponta requer conhecimento de expressões regulares e da sintaxe do Bash.

## Veja Também:

1. Documentação html-xml-utils: http://www.w3.org/Tools/HTML-XML-utils/
2. Tutorial Python BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
3. Guia de início rápido de Nokogiri: https://nokogiri.org/tutorials/parsing_an_html_xml_document.html
4. Jsoup Cookbook: https://jsoup.org/cookbook/

Lembre-se, existem muitas maneiras de fazer parsing de HTML. O importante é encontrar a ferramenta que melhor se adapta às suas necessidades.