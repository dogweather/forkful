---
title:                "Parsing HTML"
html_title:           "Javascript: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Se você está interessado em desenvolvimento web, é provável que já tenha ouvido falar em HTML, a linguagem de marcação usada para criar a estrutura de páginas da web. Mas, às vezes, pode ser necessário acessar ou manipular o código HTML de uma página de forma dinâmica. É aí que entra o parsing de HTML em Javascript.

## Como fazer

Para fazer o parsing de HTML em Javascript, você pode utilizar uma biblioteca chamada [Cheerio](https://github.com/cheeriojs/cheerio) que simplifica e agiliza esse processo. Primeiro, instale o Cheerio usando o gerenciador de pacotes npm.

```
npm install cheerio
```

Em seguida, crie um arquivo Javascript e importe o módulo do Cheerio:

```Javascript
const cheerio = require('cheerio');
```

Agora, você pode usar a função `load` do Cheerio para carregar o HTML de uma página da web em uma variável:

```Javascript
const html = '<html><body><h1>Título</h1><p>Parágrafo</p></body></html>';
const $ = cheerio.load(html);
```

A variável `$` atua como um objeto do Cheerio que representa o HTML carregado. Você pode acessar os elementos HTML e seus atributos usando seletores CSS, como no exemplo abaixo, que imprime o conteúdo da tag `<h1>`:

```Javascript
console.log($('h1').text()); // output: Título
```

## Imersão Profunda

O Cheerio permite que você navegue por um documento HTML como se estivesse usando o jQuery. Isso significa que você pode usar seletores CSS, métodos de iteração e manipulação de DOM para acessar e modificar o HTML.

Além disso, o Cheerio possui outras funcionalidades úteis, como a capacidade de fazer requisições HTTP e de encontrar elementos em páginas da web carregadas dinamicamente.

Para mais informações e documentação completa do Cheerio, confira o [site oficial](https://cheerio.js.org/).

## Veja também

- [Documentação do Cheerio](https://cheerio.js.org/)
- [Manipulando HTML com JavaScript](https://www.w3schools.com/js/js_htmldom.asp)