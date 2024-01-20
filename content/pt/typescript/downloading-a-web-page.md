---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Fazer o download de uma página da web significa buscar todos os dados de um site e armazená-lo localmente. Programadores fazem isso para analisar as informações da página, criar aplicativos de scraping de dados ou desenvolver softwares de teste.

## Como Fazer:

TypeScript simplifica o download de páginas da web com o pacote `node-fetch`. Vamos pegar a página inicial do Google, por exemplo:

```typescript
import fetch from 'node-fetch';

async function downloadPage(url: string): Promise<string> {
    const response = await fetch(url);
    if(response.ok){
        return await response.text();
    } else {
        throw new Error('Network response was not ok.');
    }
}

downloadPage('http://www.google.com')
    .then(content => console.log(content))
    .catch(error => console.log('There has been a problem with your fetch operation: ', error.message));
```
A saída será o HTML da página inicial do Google.

## Mergulho Profundo

Historicamente, fazer o download de uma página da web era muito mais complicado antes do advento de tecnologias modernas como o fetch e sua implementação no Node.js. Com a evolução da web, a necessidade de download de páginas aumentou, o que incentivou o desenvolvimento de soluções simplificadas.

Existem várias alternativas ao node-fetch, incluindo pacotes como Axios ou o módulo HTTP integrado do Node.js. 

Detalhes importantes sobre a implementação do download de web pages incluem o tratamento de erros de rede, como mostra o código de exemplo, a manipulação consistente de código de status HTTP e o gerenciamento do tempo de download, especialmente para páginas grandes ou complexas.

## Veja Também

Aqui estão alguns links para aprender mais sobre o download de páginas da web com TypeScript:

- Documentação do node-fetch: [https://github.com/node-fetch/node-fetch](https://github.com/node-fetch/node-fetch)
- Guia da Mozilla sobre a API Fetch: [https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)