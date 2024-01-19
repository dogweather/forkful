---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Enviar uma solicitação HTTP com autenticação básica é o processo de fazer um pedido a um recurso da web que requer um nome de usuário e senha. Os programadores fazem isso para garantir que apenas usuários autorizados tenham acesso a recursos específicos.

## Como Fazer:

Aqui está um exemplo de como você pode fazer isso com TypeScript usando o módulo `axios`. 

```TypeScript
import axios from 'axios';

axios({
    method: 'get',
    url: 'https://sua-api.com',
    auth: {
        username: 'seu-nome-de-usuário',
        password: 'sua-senha'
    }
}).then(response => {
    console.log(response.data);
}).catch(error => {
    console.error(error);
});
```
Quando você executa esse código, uma solicitação GET será enviada para 'https://sua-api.com' com as credenciais de autenticação especificadas. A resposta será registrada no console.

## Aprofundando um Pouco Mais

(1) Contexto Histórico: A autenticação básica é um método de autenticação de usuário na web amplamente usado e definido no HTTP/1.1. Ela foi criada em 1999, mas ainda é comumente usada devido à sua simplicidade.

(2) Alternativas: Embora a autenticação básica seja simples e amplamente suportada, ela não é a opção mais segura porque as credenciais são codificadas em Base64, mas não são criptografadas ou hashadas de alguma forma. Por isso, pode ser mais seguro usar a autenticação de token ou OAuth.

(3) Detalhes de Implementação: Quando você envia uma solicitação com autenticação básica, a informação de autenticação é colocada no cabeçalho da solicitação HTTP. O cabeçalho `Authorization` terá o valor `Basic {Base64_encode(username + ":" + password)}`.

## Veja Também:

- [MDN web docs: HTTP authentication](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
- [NPM: axios](https://www.npmjs.com/package/axios) 
- [OAuth 2.0](https://oauth.net/2/)