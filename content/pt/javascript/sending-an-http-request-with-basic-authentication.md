---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar uma solicitação HTTP com autenticação básica envolve incorporar credenciais de login em uma solicitação HTTP para autorizar o acesso a recursos protegidos. Programadores fazem isso quando querem acessar APIs que exigem autenticação de usuário.

## Como:

O código a seguir ilustra como enviar um pedido HTTP com autenticação básica usando a função `fetch` em Javascript:

```Javascript
const url = 'https://api.seusite.com';
let username = 'seuUsername';
let password = 'suaSenha';

let headers = new Headers();

headers.set('Authorization', 'Basic ' + btoa(username + ":" + password));

fetch(url, {method:'GET',
             headers: headers,
           })
.then(response => response.json())
.then(data => console.log(data))
.catch(error => console.log('Erro: ', error));
```

Neste exemplo, a função `btoa` é usada para codificar as credenciais.

## Aprofundando

A autenticação básica HTTP é um método de autenticação que foi parte do protocolo HTTP desde o início. Hoje, é considerada insegura para a maioria dos casos e está sendo substituída por métodos mais seguros como autenticação de portador de token ou OAuth.

Um detalhe importante da implementação é a necessidade de codificar as credenciais em base64 usando a função `btoa`. Isso não criptografa ou oculta as credenciais, faz parte do padrão da autenticação básica HTTP.

Além disso, a autenticação básica não fornece nenhum mecanismo de logoff. Quando a autenticação é necessária novamente, o navegador simplesmente reenvia as credenciais que já possui.

## Veja também

- [MDN Web Docs: Usando Fetch](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API/Using_Fetch)
- [MDN Web Docs: Autenticação HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
- [Autenticação básica HTTP no Wikipedia](https://pt.wikipedia.org/wiki/Autentica%C3%A7%C3%A3o_b%C3%A1sica_por_HTTP)