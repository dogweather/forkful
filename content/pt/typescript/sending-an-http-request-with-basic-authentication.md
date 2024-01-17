---
title:                "Enviando uma solicitação http com autenticação básica."
html_title:           "TypeScript: Enviando uma solicitação http com autenticação básica."
simple_title:         "Enviando uma solicitação http com autenticação básica."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?

Enviar uma solicitação HTTP usando autenticação básica é uma forma de garantir que suas informações on-line sejam protegidas. Os programadores geralmente fazem isso para acessar e manipular dados protegidos por meio de APIs, como o acesso a dados bancários ou informações de usuários em um aplicativo.

## Como fazer:

Utilizando o TypeScript, você pode enviar uma solicitação HTTP com autenticação básica usando a biblioteca nativa `http`. Aqui está um exemplo simples de código:

```TypeScript
import { request } from "http";

const username = "user123";
const password = "pass456";
const auth = "Basic " + Buffer.from(username + ":" + password).toString("base64");

const options = {
  hostname: "api.example.com",
  port: 3000,
  path: "/endpoint",
  method: "GET",
  headers: {
    "Authorization": auth
  }
};

const req = request(options, response => {
  response.on("data", data => {
    console.log(data.toString());
  });
});

req.on("error", error => {
  console.error(error);
});

req.end();
```

Este código faz uma solicitação GET para a rota `/endpoint` em `api.example.com` com autenticação básica, passando o nome de usuário e senha como credenciais.

## Deep Dive:

A autenticação básica é um método de autenticação antigo amplamente utilizado, mas foi substituída por métodos mais seguros, como o OAuth. No entanto, ainda é usada em APIs legadas e em alguns casos de uso específicos.

Uma alternativa à autenticação básica é o uso de tokens de segurança, em que um token criptografado é usado em vez de nome de usuário e senha. Isso é mais seguro, pois o token pode ser revogado a qualquer momento, se necessário, enquanto as credenciais de autenticação básicas permanecem as mesmas.

Implementar a autenticação básica envolve base64 encoding das credenciais de autenticação, que é uma forma básica de codificar dados em formato ASCII. É importante lembrar que este método não criptografa suas credenciais, por isso pode ser interceptado por invasores.

## Veja também:

- [Documentação da biblioteca HTTP nativa do Node.js](https://nodejs.org/api/http.html)
- [Detalhes sobre autenticação básica](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
- [Alternativas à autenticação básica](https://www.digitalocean.com/community/tutorials/should-you-still-use-basic-authentication)