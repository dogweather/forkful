---
title:                "PHP: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Parsear HTML é uma tarefa essencial para qualquer desenvolvedor PHP que trabalhe com a web. Ao fazer isso, você pode extrair informações valiosas de sites, como dados de vendas, preços, informações de contato e muito mais. Além disso, você pode automatizar tarefas, como buscar e indexar informações, o que economiza tempo e esforço.

## Como fazer

Antes de começar a parsear HTML, você precisa ter familiaridade com a linguagem de programação PHP e também com a estrutura do HTML. Você também precisará de uma biblioteca chamada Simple HTML DOM Parser, que é usada para extrair dados do HTML.

Aqui está um exemplo básico de como usar o Simple HTML DOM Parser para extrair o texto de um elemento HTML específico:

```
<?php
  // Incluindo o arquivo da biblioteca Simple HTML DOM Parser
  include('simple_html_dom.php');

  // Carregando a página a ser parseada
  $html = file_get_html('http://www.exemplo.com/');

  // Encontrando o elemento desejado
  $element = $html->find('.titulo', 0);

  // Imprimindo o texto do elemento
  echo $element->plaintext;
?>
```

Isso irá imprimir o texto do primeiro elemento com a classe "titulo" encontrado no HTML.

## Mergulho profundo

Além de extrair informações de elementos específicos no HTML, você também pode usar o Simple HTML DOM Parser para percorrer todo o documento e coletar dados de diferentes seções. Por exemplo, você pode usar a função `find()` e especificar uma tag HTML para obter uma lista de todos os elementos com essa tag. Em seguida, você pode percorrer essa lista usando um loop `for` ou `foreach` e obter as informações que precisa.

Também é possível usar os recursos avançados do PHP, como expressões regulares, para fazer parsing de HTML. No entanto, isso requer conhecimentos mais avançados de programação e pode ser mais complicado do que o uso do Simple HTML DOM Parser.

## Veja também

- [Documentação do Simple HTML DOM Parser](http://simplehtmldom.sourceforge.net/manual.htm)
- [Tutorial de parseamento de HTML com PHP](https://www.tutorialrepublic.com/php-tutorial/php-parse-html-document.php)
- [Vídeo tutorial de parseamento de HTML com PHP](https://youtu.be/wk88FzBt9JQ)