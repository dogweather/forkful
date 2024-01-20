---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma solicitação HTTP é o ato de pedir ao servidor para retornar data específica. Como programadores, fazemos isso para obter, postar, atualizar ou excluir data em um servidor remoto.

## Como fazer:
Vamos usar a ferramenta `fetch` integrada para enviar uma solicitação GET ao endpoint 'https://jsonplaceholder.typicode.com/users':

```TypeScript
fetch('https://jsonplaceholder.typicode.com/users')
  .then(response => response.json())
  .then(users => console.log(users));
```
Aqui está um exemplo de output:
```TypeScript
[
 {id: 1, name: "Leanne Graham", ...},
 {id: 2, name: "Ervin Howell", ...},
 ...
]
```
Para enviar uma solicitação POST para criar um novo usuário:
```TypeScript
let user = {
    name: "Novo Usuario",
    username: "novo_usuario",
    email: "novo_usuario@email.com"
};

fetch('https://jsonplaceholder.typicode.com/users', {
    method: 'POST',
    body: JSON.stringify(user),
    headers: {
        'Content-type': 'application/json; charset=UTF-8'
    }
})
.then(response => response.json())
.then(json => console.log(json));
```

## Visão Detalhada
Historicamente, as solicitações HTTP eram feitas usando o objeto `XMLHttpRequest`. `fetch` é uma alternativa mais moderna e robusta.

Uma alternativa ao `fetch` é a biblioteca `axios`. É uma biblioteca de cliente HTTP que implementa promises e está disponível para o Navegador e para o Node.js. Ela tem uma resposta de erro superior e uma configuração global de fácil uso.

A maior parte do trabalho ao enviar uma solicitação HTTP é lidar com os dados de retorno. Isso geralmente envolve a conversão da resposta em JSON (`response.json()`) e a manipulação dos dados resultantes.

## Veja Também
- [Guia de Javascript Fetch API - Mozilla Developer Network](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [Documentação do axios](https://axios-http.com/)
- [JSON Placeholder - Para testar as solicitações HTTP](https://jsonplaceholder.typicode.com/)