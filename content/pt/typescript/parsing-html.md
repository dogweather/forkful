---
title:                "Análise de HTML"
html_title:           "TypeScript: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que fazer o parsing de HTML?

O parsing de HTML é o processo de analisar um documento HTML e extrair informações específicas dele. Os programadores fazem isso para obter dados estruturados de uma página da web ou para validar se o HTML está seguindo padrões corretos.

## Como fazer:

```TypeScript
// Exemplo de parsing de HTML usando a biblioteca Cheerio.
import * as cheerio from 'cheerio';

// Carregando o HTML de uma página com o módulo "request".
const request = require('request');
request('https://www.google.com.br', (error, response, html) => {
  if (!error && response.statusCode == 200) {
    
    // Carregar o HTML usando Cheerio
    const $ = cheerio.load(html);
    
    // Obter o título da página
    const title = $('title').text();
    
    // Imprimir o resultado
    console.log(title); // "Google"
  }
});
```

## Mergulho profundo:

O parsing de HTML é uma técnica antiga, que tem sido usada há décadas para extrair dados de páginas da web. Existem muitas bibliotecas disponíveis para facilitar esse processo, como o Cheerio, mostrado no exemplo acima. Outra alternativa é utilizar uma API de web scraping, que pode oferecer recursos mais avançados, mas geralmente requerem um pagamento.

## Veja também:

- [Documentação oficial da biblioteca Cheerio] (https://cheerio.js.org/)
- [API de web scraping do ParseHub] (https://www.parsehub.com/api/)
- [Guia passo a passo para fazer o parsing de HTML com TypeScript] (https://blog.bitsrc.io/extracting-data-from-html-using-ts-neo4j-f28ae1f6afcf)