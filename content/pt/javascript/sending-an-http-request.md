---
title:                "Enviando uma solicitação http"
html_title:           "Javascript: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?

Sempre que você acessa um site, o seu navegador envia uma solicitação HTTP para o servidor do site. Em termos simples, enviar uma solicitação HTTP significa pedir para ter acesso a algum conteúdo ou interagir com um servidor web. Programadores usam isso para criar funcionalidades dinâmicas em seus aplicativos e fazer com que eles se comuniquem com outros servidores para acessar e enviar dados.

## Como fazer:

Você pode enviar uma solicitação HTTP usando a função `fetch()` no JavaScript. Por exemplo:
```javascript
fetch('https://api.example.com/users')
  .then(response => response.json())
  .then(data => console.log(data));
```
Neste exemplo, estamos enviando uma solicitação para a URL `https://api.example.com/users` e usando o método `.json()` para formatar a resposta recebida em um objeto JSON.

## Mergulho profundo:

A sigla HTTP significa "Hypertext Transfer Protocol" e foi desenvolvida em 1991 para permitir a troca de informações entre navegadores e servidores web. Antes da criação do HTTP, as comunicações eram feitas apenas através do protocolo de rede TCP/IP. Existem outras formas de enviar solicitações HTTP, como por exemplo, usando as bibliotecas `axios` ou `request` no Node.js.

## Veja também:

- [MDN Web Docs: Como funciona o protocolo HTTP?](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview)
- [Axios](https://github.com/axios/axios)
- [Request](https://github.com/request/request)