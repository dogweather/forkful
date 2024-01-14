---
title:                "Javascript: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que: 
Uma das razões mais comuns para uma pessoa utilizar o envio de uma requisição HTTP com autenticação básica é para acessar recursos ou informações confidenciais em uma aplicação web. Isso garante que apenas usuários autorizados tenham acesso a esses dados, tornando a comunicação mais segura.

## Como fazer:
Para enviar uma requisição HTTP com autenticação básica em Javascript, você precisará utilizar o método `fetch`. Primeiro, crie um objeto `Headers` contendo as informações de autenticação, como o nome de usuário e senha. Em seguida, passe esse objeto como argumento para o método `fetch`, junto com a URL do recurso que você deseja acessar. Veja um exemplo abaixo:

```Javascript
const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa('seunomeusuario:suasenha'));
const url = 'https://www.exemplo.com/recurso-seguro';
fetch(url, {
  method: 'GET',
  headers: headers
})
.then(response => console.log(response))
.catch(error => console.log(error));
```
Neste exemplo, estamos acessando um recurso seguro na URL `https://www.exemplo.com/recurso-seguro`, incluindo as credenciais de autenticação no cabeçalho da requisição. Ao chamar o método `fetch`, você receberá uma resposta com as informações solicitadas, que pode ser manipulada de acordo com suas necessidades.

## Aprofundando:
A autenticação básica é um dos métodos de autenticação mais simples, porém não é o mais seguro. As credenciais de autenticação são codificadas em ASCII e podem ser facilmente decodificadas por programas maliciosos. Além disso, essas informações são enviadas em texto não criptografado, tornando-as vulneráveis a ataques de 'man-in-the-middle'.

Para melhorar a segurança ao utilizar autenticação básica, recomenda-se utilizar o protocolo HTTPS, que criptografa todas as informações trocadas entre o cliente e o servidor. Outra opção seria adicionar uma camada extra de segurança, como um token de autenticação, que é gerado e renovado a cada requisição.

Lembre-se também de nunca armazenar informações de autenticação em código Javascript, pois elas podem ser facilmente acessadas por qualquer pessoa.

## Veja também:
- [Método fetch() em JavaScript](https://developer.mozilla.org/pt-BR/docs/Web/API/Fetch_API)
- [Autenticação HTTP básica](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
- [Autenticação HTTP com HTTPS](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#Limita%C3%A7%C3%B5es_do_esquema_Autentica%C3%A7%C3%A3o_B%C3%A1sica)