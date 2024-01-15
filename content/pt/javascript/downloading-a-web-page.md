---
title:                "Baixando uma página da web"
html_title:           "Javascript: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Baixar uma página da web é uma tarefa comum para desenvolvedores web. Isso pode ser necessário para extrair informações de uma página, fazer testes de integração ou armazenar um site offline.

## Como fazer

Caso você esteja usando o Node.js, você pode baixar uma página da web facilmente usando o módulo `request`. Aqui está um exemplo de como baixar a página inicial do Google e imprimir o conteúdo em seu terminal:

```Javascript
const request = require('request');
request('https://www.google.com', function(err, res, body) {
  console.log(body);
});
```

O exemplo acima faz uma chamada HTTP para o URL especificado e retorna o conteúdo da página no parâmetro `body`. Você pode alterar o URL para baixar qualquer página da web que desejar.

## Mergulho Profundo

Existem diferentes maneiras de baixar uma página da web em Javascript, como usando a API `Fetch` ou o módulo` http`. Além disso, você pode especificar cabeçalhos HTTP personalizados ou usar autenticação para fazer o download de páginas restritas. Também é importante considerar a segurança ao fazer chamadas HTTP, pois dados sensíveis podem ser expostos.

## Veja também

- [Módulo request no NPM](https://www.npmjs.com/package/request)
- [API Fetch no MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [Módulo HTTP no Node.js](https://nodejs.org/api/http.html)