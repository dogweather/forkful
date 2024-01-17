---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Javascript: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?

Enviar uma solicitação HTTP com autenticação básica é um processo importante para os programadores. Trata-se de enviar uma solicitação para um servidor com informações de autenticação (como nome de usuário e senha) para acessar dados protegidos. Isso é necessário para assegurar que apenas usuários autorizados possam visualizar as informações.

## Como fazer:

```Javascript
// Importando o módulo "http" do Node.js
const http = require('http');

// Definindo as informações de autenticação
const username = 'exemplo';
const password = 'senha123';

// Definindo a URL e o método da requisição
const url = 'https://www.exemplo.com/dados';
const method = 'GET';

// Criando a string de autenticação
const authString = `${username}:${password}`;

// Codificando a string de autenticação em Base64
const encodedAuth = Buffer.from(authString).toString('base64');

// Definindo as opções para a requisição
const options = {
  url,
  method,
  headers: {
    'Authorization': `Basic ${encodedAuth}` // Adicionando a autenticação ao header da requisição
  }
};

// Enviando a requisição
http.request(options, (res) => {
  // Lendo a resposta e imprimindo o conteúdo
  res.on('data', (data) => {
    console.log(`${data}`);
  });
}).end();
```

Saída:

```
// Conexão bem sucedida
{
  data: [dados protegidos]
}
```

## Mergulho profundo:

A autenticação básica é um padrão de autenticação para acesso a recursos da web. Foi criada no início dos anos 90 para fornecer uma forma simples de autenticação em aplicações web. Apesar de ser utilizada amplamente, a autenticação básica não é considerada segura, pois envia as informações de autenticação sem criptografia.

Existem alternativas mais seguras, como a autenticação baseada em token, que utiliza um token único e criptografado para autenticar as solicitações. Além disso, é importante implementar medidas extras de segurança, como SSL, para garantir a proteção dos dados transmitidos.

## Veja também:

- [Como funciona a autenticação básica](https://www.infoq.com/br/articles/autenticao-web/)
- [Como implementar autenticação básica em Node.js] (https://gist.github.com/cdaringe/6e83bc3caaa3a7e3262c)