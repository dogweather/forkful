---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Baixar uma página da web é o processo de coletar e armazenar os dados de uma página para análise e uso posterior. Programadores fazem isso para colher informações, verificar alterações ou realizar testes.

## Como Fazer:

```PHP
<?php 
$file = 'página.html';
file_put_contents($file, fopen('http://www.exemplo.com', 'r'));
?>
```

Isso baixará a página web `www.exemplo.com` e a armazenará em um arquivo chamado `página.html`.

## Deep Dive:

Originalmente, baixar páginas da web era uma tarefa comum para bots de web crawling, usados para indexar o conteúdo da internet para motores de busca. Hoje, ainda é usado para esse fim, além de monitoramento, testes e análise de dados.

Uma alternativa ao método PHP é usar ferramentas de linha de comando como `wget` ou `curl`. Estes podem ser mais adequados para casos de uso mais complexos, como lidar com autenticação.

No que diz respeito aos detalhes de implementação, `fopen` e `file_put_contents` lidam com o streaming e gravação da página, respectivamente. Há muito mais que pode ser feito aqui para lidar com erros, configurar timeouts, e assim por diante.

## Ver Também:

Para mais formas de baixar páginas da web em PHP, visite: [este link](https://www.php.net/manual/pt_BR/function.fopen.php)

Para uma visão geral do web scraping (a prática de baixar e analisar páginas da web) em PHP, visite: [este link](https://www.php.net/manual/pt_BR/book.dom.php)