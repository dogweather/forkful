---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Baixando uma Página Web em Javascript

## O que & Por quê?

Baixar uma página da web significa obter o conteúdo HTML de um site. Os programadores fazem isso para acessar, manipular ou armazenar os dados de um site.

## Como Fazer:

Para baixar uma página web em Javascript, você pode usar o pacote 'axios'. Aqui está um exemplo:

```Javascript
const axios = require('axios');

async function baixarPagina(url) {
    const resposta = await axios.get(url);
    return resposta.data;
}

baixarPagina('https://www.exemplo.com').then(console.log);
```

Isso imprimirá o HTML da página 'https://www.exemplo.com' no console.

## Mergulho Profundo

Historicamente, a ação de baixar páginas da web estava no coração da web scraping, uma prática que remonta à infância da internet. Anteriormente, as bibliotecas como 'request' eram comumente usadas, mas agora estão obsoletas.

Hoje, pacotes como 'axios' são preferidos devido à sua facilidade de uso e promessas de suporte. No entanto, há outras alternativas, como 'node-fetch', que emula a API Fetch dos navegadores web.

Ao baixar uma página da web, be aware with terms of use from websites. São as regras que devem seguir para não infringir os direitos of the site.

## Veja Também

- Documentação do 'axios': https://github.com/axios/axios
- 'node-fetch' no npm: https://www.npmjs.com/package/node-fetch
- Wikipédia sobre Web Scraping: https://pt.wikipedia.org/wiki/Coleta_de_dados_web