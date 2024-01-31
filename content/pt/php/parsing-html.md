---
title:                "Análise de HTML"
date:                  2024-01-20T15:33:01.790293-07:00
simple_title:         "Análise de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Parsear HTML é o processo de transformar código HTML em algo que um programa possa entender e manipular. Programadores fazem isso para extrair dados, manipular conteúdo ou integrar sistemas.

## Como Fazer:

Vamos usar o DOMDocument para parsear HTML em PHP.

```PHP
<?php
$doc = new DOMDocument();
libxml_use_internal_errors(true); // Evita warnings com HTML malformado
$doc->loadHTML('<html><body><p>Oi, pessoal!</p></body></html>');
$paragrafos = $doc->getElementsByTagName('p');

foreach ($paragrafos as $p) {
    echo $p->nodeValue . PHP_EOL;
}
?>
```

Saída de amostra:

```
Oi, pessoal!
```

## Aprofundando:

No passado, parsear HTML era um terreno minado: muita gente usava expressões regulares, que são traiçoeiras para esse fim. O DOMDocument, por outro lado, proporciona uma interface segura e robusta.

Alternativas incluem SimpleXML para estruturas menos complexas e bibliotecas de terceiros como PHP Simple HTML DOM Parser, que é mais indulgente com HTML imperfeito.

A implementação de parseamento utiliza o libxml2, proporcionando uma forma eficaz de trabalhar com HTML e XML. Mesmo que o HTML esteja com tags despareadas ou falhas, o libxml_use_internal_errors(true) nos permite trabalhar sem sermos interrompidos por erros menores.

## Veja Também:

- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP Simple HTML DOM Parser](https://simplehtmldom.sourceforge.io/)
- [W3Schools PHP DOM Parser](https://www.w3schools.com/php/php_xml_dom.asp)
