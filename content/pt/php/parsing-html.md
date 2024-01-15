---
title:                "Analisando html"
html_title:           "PHP: Analisando html"
simple_title:         "Analisando html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Por que alguém se envolveria em analisar HTML? É importante entender que, ao trabalhar com dados na web, muitas vezes precisamos extrair informações de páginas HTML para usá-las em nosso código. Isso é especialmente útil para coletar dados de páginas da web para fins de análise ou para criar scripts automatizados para ações específicas.

## Como fazer

A maneira mais comum de analisar HTML em PHP é usando a biblioteca nativa do PHP, o DOMDocument. Vamos dar uma olhada em um exemplo simples de como usar o DOMDocument para analisar uma página HTML:

```PHP
<?php
// Faz o download da página HTML
$html = file_get_contents('https://www.example.com');

// Cria um novo DOMDocument
$dom = new DOMDocument();

// Carrega a página HTML baixada no DOMDocument
$dom->loadHTML($html);

// Procura por um elemento específico, por exemplo, um título <h1>
$title = $dom->getElementsByTagName('h1')->item(0)->textContent;

// Exibe o título
echo $title;
```

Neste exemplo, usamos a função `file_get_contents` para baixar a página HTML de exemplo. Em seguida, criamos um novo objeto DOMDocument e carregamos o HTML baixado nele usando o método `loadHTML`. Depois disso, usamos o método `getElementsByTagName` para encontrar o elemento de título `<h1>` e, por fim, imprimimos o seu conteúdo usando o método `textContent`.

### Saída de exemplo

Se tudo funcionou corretamente, a saída desse exemplo seria algo como:

```
Welcome to Example.com
```

Claro, este é apenas um exemplo simples de como analisar HTML usando o DOMDocument. Existem muitas outras funções e métodos disponíveis para trabalhar com o DOMDocument, permitindo uma análise mais avançada de páginas HTML.

## Mergulho profundo

A biblioteca DOMDocument nos oferece uma grande variedade de métodos e funções para analisar e manipular HTML. Alguns dos mais importantes são:

- `loadHTML()` - Carrega o HTML em um objeto DOMDocument.
- `getElementsByTagName()` - Retorna uma lista de elementos com uma tag HTML específica.
- `getAttribute()` - Obtém o valor de um atributo de um elemento.
- `setAttribute()` - Define o valor de um atributo de um elemento.
- `textContent` - Obtém ou define o conteúdo de texto de um elemento.

Para saber mais sobre todas as funções disponíveis, é possível consultar a documentação oficial do PHP.

## Veja também

Aqui estão alguns links úteis para continuar aprendendo sobre como analisar HTML em PHP:

- [Documentação oficial do PHP para a biblioteca DOMDocument](https://www.php.net/manual/pt_BR/book.dom.php)
- [Tutorial da DigitalOcean sobre parsing de HTML em PHP](https://www.digitalocean.com/community/tutorials/how-to-parse-html-in-php)
- [Vídeo do YouTube sobre como usar o DOMDocument em PHP](https://www.youtube.com/watch?v=EoRv2fg76Qs)