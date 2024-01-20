---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar um pedido HTTP é o processo que permite que o seu programa converse com servidores e APIs. Isso volta-se crucial quando precisamos puxar, enviar ou manipular dados de servidores remotos.

## Como Fazer:
Para começar, podemos usar uma função interna no JavaScript, o `fetch()`. Aqui está como você faria para pegar dados de uma API:

```Javascript
fetch('https://api.example.com/data')
  .then(resposta => resposta.json())
  .then(dados => console.log(dados))
  .catch(erro => console.error('Erro:', erro));
```

O código acima vai imprimir no console os dados que ele puxou da URL da API.

## Mergulho Profundo

Enviar pedidos HTTP não é uma nova funcionalidade e tem sido um pilar na programação web. Antes do `fetch()`, o objeto `XMLHttpRequest` era comumente utilizado. Hoje em dia, também temos alternativas como o `axios` e o `jQuery.ajax()`.

O `fetch()`, contudo, é nativo no JavaScript e retorna Promises, o que torna muito mais fácil trabalhar com operações assíncronas. Aqui está um exemplo de como você poderia usar o `fetch()` para postar dados em um servidor:

```Javascript
let dados = { nome: "João" };

fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify(dados)
})
.then(resposta => resposta.json())
.then(dados => console.log('Dados salvos com sucesso:', dados))
.catch((erro) => console.error('Erro:', erro));
```

## Veja Também

- [Documentação da Fetch API na MDN](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [Comparação entre fetch e XMLHttpRequest](https://developers.google.com/web/updates/2015/03/introduction-to-fetch#_1)
- [Biblioteca axios JavaScript](https://github.com/axios/axios)
- [Função jQuery.ajax()](https://api.jquery.com/jquery.ajax/)