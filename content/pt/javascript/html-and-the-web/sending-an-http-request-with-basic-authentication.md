---
title:                "Enviando uma requisição HTTP com autenticação básica"
aliases:
- /pt/javascript/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:10.606316-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Enviar um pedido HTTP com autenticação básica significa incluir credenciais de usuário e senha codificados em base-64 no cabeçalho do pedido. Programadores fazem isso para acessar recursos protegidos em um servidor remotamente.

## Como Fazer:
```Javascript
// Utilizando o módulo 'axios' para fazer o pedido HTTP com autenticação básica.
const axios = require('axios');
const base64Credentials = Buffer.from('usuario:senha').toString('base64');

axios.get('https://algumsite.com/dados', {
  headers: {
    'Authorization': `Basic ${base64Credentials}`
  }
})
.then(response => {
  console.log('Dados recebidos:', response.data);
})
.catch(error => {
  console.error('Erro na requisição:', error);
});
```
Saída esperada (sample output):
```
Dados recebidos: { ... } // Dados retornados do servidor
```

## Mergulho Profundo
A autenticação básica é um protocolo simples de autenticação incluído nos padrões HTTP iniciados nos anos 90. Alternativas mais seguras, como OAuth e JWT (JSON Web Tokens), são recomendadas para novas aplicações devido a uma maior segurança. Na autenticação básica, usuário e senha são codificados, mas não criptografados, o que pode ser um risco de segurança se não utilizado com HTTPS. A implementação acima é direta em Node.js usando o axios, mas pode ser adaptada para outros ambientes Javascript.

## Veja Também
- MDN Web Docs sobre autenticação HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
- Comparação entre autenticação básica e OAuth: https://auth0.com/docs/authoriza/authenticate/http-authentication/basic-auth-vs-oauth
- Documentação do axios: https://axios-http.com/docs/intro
