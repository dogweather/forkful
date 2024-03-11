---
date: 2024-01-20 18:00:47.344770-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP \xE9 fundamentalmente o ato de solicitar\
  \ dados de um servidor remoto. Programadores fazem isso para interagir com APIs,\
  \ recuperar\u2026"
lastmod: '2024-03-11T00:14:20.007930-06:00'
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP \xE9 fundamentalmente o ato de solicitar\
  \ dados de um servidor remoto. Programadores fazem isso para interagir com APIs,\
  \ recuperar\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP"
---

{{< edit_this_page >}}

## What & Why?
Enviar uma requisição HTTP é fundamentalmente o ato de solicitar dados de um servidor remoto. Programadores fazem isso para interagir com APIs, recuperar recursos da web ou comunicar-se entre diferentes serviços.

## How to:
Para enviar uma requisição HTTP em TypeScript, você pode usar a API `fetch` nativa ou bibliotecas como Axios. Vamos ver um exemplo simples com `fetch` e outro com Axios:

```TypeScript
// Exemplo com Fetch API
fetch('https://api.exemplo.com/data')
  .then(response => {
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return response.json();
  })
  .then(data => console.log(data))
  .catch(error => console.error('Fetch error:', error));

// Exemplo com Axios
import axios from 'axios';

axios.get('https://api.exemplo.com/data')
  .then(response => console.log(response.data))
  .catch(error => console.error('Axios error:', error));
```

O `fetch` retorna uma promessa que, quando resolvida, te dá acesso aos dados da requisição. O Axios simplifica um pouco o processo, tratando automaticamente a conversão para JSON.

## Deep Dive
O envio de requisições HTTP é um dos pilares da comunicação na web, surgindo com a própria internet. Antes de `fetch` e Axios, o objeto `XMLHttpRequest` era comumente usado, mas seu uso caiu em desuso por causa da sintaxe mais complexa e menos intuitiva. 

Outras bibliotecas, como o jQuery, também ofereceram suas soluções para realizar requisições, mas com o avanço das APIs nativas de JavaScript e do suporte dos navegadores, muitos desenvolvedores migraram para soluções mais modernas e leves.

Quanto aos detalhes de implementação, é importante entender conceitos como CORS (Cross-Origin Resource Sharing), que é uma política de segurança da web. Ela restringe como recursos podem ser carregados de diferentes origens, o que é crucial quando se lida com APIs de terceiros.

## See Also
- Documentação do Fetch API: [MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- Documentação do Axios: [GitHub](https://github.com/axios/axios)
- Entendendo CORS: [MDN](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/CORS)
- Polyfill para Fetch API (para suporte em navegadores mais antigos): [GitHub](https://github.com/github/fetch)
