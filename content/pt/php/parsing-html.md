---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-html.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Analisar HTML (parse HTML) é o processo de decodificar a estrutura de uma página web. Fazemos isso para extrair informações úteis das páginas e manipulá-las de maneira programada.

## Como fazer:

Vamos usar a biblioteca de PHP Simple HTML DOM Parser para esta tarefa. Primeiro, você precisa instalá-la usando o composer:

```PHP
composer require simplehtmldom/simplehtmldom
```

Agora, vamos analisar um código HTML simples:

```PHP
<?php
require_once __DIR__ . '/vendor/autoload.php';

use simplehtmldom\simple_html_dom;

$html = "<html><body><p>Olá, mundo!</p></body></html>";

$dom = new simple_html_dom();
$dom->load($html);

$p = $dom->find('p', 0);
echo $p->innertext; // output: Olá, mundo!
```

Este script procura o primeiro elemento `<p>` na nossa string HTML e imprime seu conteúdo.

## Uma análise mais profunda

Analisar HTML existe desde os primeiros dias da web, com o objetivo de obter qualquer tipo de informação embutida na estrutura HTML.

Um método alternativo ao Simple HTML DOM Parser seria usar o PHP's DOM e XPath. Aqui está um exemplo de como fazer isso:

```PHP
$dom = new DOMDocument();
$dom->loadHTML($html);
$xpath = new DOMXPath($dom);

$p = $xpath->query('//p')->item(0);
echo $p->nodeValue; // output: Olá, mundo!
```

Por fim, é importante ter em mente que a análise HTML é uma operação intensiva em termos de recursos, mesmo para pequenos documentos HTML. Portanto, é melhor usá-la com moderação.

## Veja também

Se você quiser aprender mais sobre análise HTML, aqui estão alguns links úteis:

- Documentação do Simple HTML DOM Parser: [http://simplehtmldom.sourceforge.net/](http://simplehtmldom.sourceforge.net/)
- PHP DOM: [https://www.php.net/manual/en/book.dom.php](https://www.php.net/manual/en/book.dom.php)
- XPath tutorial: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)