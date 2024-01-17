---
title:                "Analisando HTML"
html_title:           "PHP: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que os programadores fazem isso?

O parsing HTML é o processo de analisar o código HTML de uma página da web para extrair informações específicas. Os programadores geralmente o fazem para coletar dados para um aplicativo ou para automatizar tarefas repetitivas.

## Como fazer:

```PHP
// Exemplo de código para fazer parsing HTML e retornar o título de uma página
<?php
  $html = file_get_contents('https://www.example.com/');
  preg_match("/<title>(.*)<\/title>/siU", $html, $matches);
  echo $matches[1];
?>
```

Output: ```Exemplo de Página```

## Profundando:

O processo de parsing HTML tem sido usado desde os primeiros dias da internet para extrair informações de sites. Existem várias ferramentas e bibliotecas disponíveis para facilitar esse processo, como o PHP Simple HTML DOM Parser. Além disso, alguns programadores optam por usar outras linguagens, como Python, para fazer o parsing de HTML.

## Veja também:

- [PHP Simple HTML DOM Parser](https://simplehtmldom.sourceforge.io/)
- [Python Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)