---
title:                "TypeScript: Enviando uma requisição http com autenticação básica"
simple_title:         "Enviando uma requisição http com autenticação básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que utilizar autenticação básica em requisições HTTP?

A autenticação básica é um método simples e eficaz para garantir que apenas usuários autorizados acessem determinados recursos em uma aplicação. Ao enviar uma requisição HTTP com autenticação básica, o usuário deve fornecer um nome de usuário e senha para comprovar sua identidade antes de receber uma resposta da aplicação. Isso ajuda a garantir que apenas usuários autorizados possam acessar informações sensíveis ou realizar ações específicas.

## Como enviar uma requisição HTTP com autenticação básica em TypeScript

Para enviar uma requisição HTTP com autenticação básica em TypeScript, primeiro precisamos importar o módulo `https` nativo do Node.js. Então, definimos as credenciais de autenticação em uma variável de cabeçalho, que será anexada à nossa requisição. Por fim, usamos o método `request` do módulo `https` para enviar a requisição ao servidor.

~~~TypeScript
import https from 'https';

const username = 'fulano';
const password = '012adc34e';
const authHeader = `Basic ${Buffer.from(`${username}:${password}`).toString('base64')}`;

const options = {
  hostname: 'www.exemplo.com',
  path: '/recurso',
  headers: {
    'Authorization': authHeader
  }
};

https.request(options, (res) => {
  res.on('data', (chunk) => {
    console.log(`Dados recebidos: ${chunk}`);
  });
}).end();
~~~

Ao executar este código, a aplicação enviará uma requisição HTTP GET para o endereço `www.exemplo.com/recurso` e retornará os dados recebidos no console.

## Aprofundando-se em requisições HTTP com autenticação básica em TypeScript

Além da autenticação básica, existem outros métodos de autenticação como o Digest e o Bearer, que também podem ser usados em requisições HTTP. Além disso, as especificações do HTTP também permitem o uso de outros tipos de autenticação baseados em tokens, como o OAuth. É importante estar familiarizado com esses métodos e escolher o mais apropriado para cada situação.

Outro aspecto importante a se considerar é que, ao usar a autenticação básica, as credenciais são enviadas em formato de texto simples, o que torna vulnerável a interceptação por hackers. Portanto, é importante garantir que a conexão esteja criptografada com o uso de HTTPS para proteger as informações de autenticação.

## Veja também
[Documentação do módulo HTTPS do Node.js](https://nodejs.org/api/https.html)

[Artigo sobre autenticação HTTP da Mozilla](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Basic_access_authentication)

[Artigo sobre autenticação com tokens da Microsoft](https://msdn.microsoft.com/pt-br/library/azure/dn645542.aspx)