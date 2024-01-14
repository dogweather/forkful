---
title:                "TypeScript: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Por que?

A análise de HTML é uma habilidade importante para qualquer programador ou desenvolvedor web. Ao aprender como analisar e extrair informações de documentos HTML, é possível criar aplicações mais dinâmicas e personalizadas, além de automatizar tarefas repetitivas. Esta postagem irá explicar como realizar análise de HTML usando TypeScript.

## Como fazer

Para realizar análise de HTML em TypeScript, é necessário primeiro instalar a biblioteca npm `cheerio`. Em seguida, é preciso importar a biblioteca e usar a função `load` para carregar um documento HTML. Veja um exemplo abaixo:

```TypeScript
import * as cheerio from 'cheerio';

const html = `
  <html>
    <head>
      <title>Exemplo</title>
    </head>
    <body>
      <h1>Cabeçalho</h1>
      <p>Parágrafo</p>
    </body>
  </html>
`;

const $ = cheerio.load(html); // carrega o HTML

// usando o seletor CSS para obter o conteúdo do título
const title = $('head > title').text();
console.log(title); // output: Exemplo

// usando o seletor CSS para obter o conteúdo do parágrafo
const paragraph = $('body > p').text();
console.log(paragraph); // output: Parágrafo
```

Na primeira linha, importamos a biblioteca `cheerio` e, em seguida, usamos a função `load` para carregar o HTML na constante `$`. A partir daí, podemos usar seletores CSS para obter o conteúdo desejado do documento HTML. O resultado será exibido no console.

## Deep Dive

A biblioteca `cheerio` é uma excelente opção para analisar e manipular documentos HTML em TypeScript. Ela permite usar seletores CSS para obter conteúdo específico do documento e também possui funções para navegar na árvore do DOM.

Além disso, a biblioteca também suporta a manipulação de documentos HTML invalidados, tornando-a uma opção robusta para análise de documentos em tempo real.

## Veja também

- Documentação do Cheerio: https://cheerio.js.org/
- Tutorial de análise de HTML com Cheerio: https://medium.com/@scottschweigert/parsing-html-with-typescript-and-cheerio-fd0e89811ae8