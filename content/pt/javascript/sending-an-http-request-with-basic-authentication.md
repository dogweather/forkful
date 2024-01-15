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

## Por que

Enviar uma solicitação HTTP com autenticação básica é uma forma segura de se comunicar com servidores externos. Isso permite que apenas usuários autorizados tenham acesso às informações solicitadas, protegendo os dados sensíveis.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Javascript, é necessário seguir alguns passos simples:

1. Primeiramente, crie uma instância de um objeto `XMLHttpRequest`, que é responsável por fazer chamadas HTTP assíncronas.

```Javascript
let request = new XMLHttpRequest();
```

2. Em seguida, defina o método e a URL que será utilizada para enviar a requisição.

```Javascript
request.open('GET', 'http://www.example.com/api/data');
```

3. Adicione as credenciais de autenticação no cabeçalho da solicitação utilizando o método `setRequestHeader()`, passando o nome do cabeçalho e um valor codificado em Base64 com o padrão `username:password`.

```Javascript
let username = 'user123';
let password = 'pass456';

request.setRequestHeader('Authorization', 'Basic ' + btoa(username + ':' + password));
```

4. Por fim, envie a solicitação utilizando o método `send()` e trate a resposta no evento `onreadystatechange`.

```Javascript
request.send();

request.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
        // A resposta do servidor pode ser acessada no atributo responseText
        console.log(this.responseText);
    }
};
```

O resultado será a resposta da requisição, que pode ser utilizada para exibir os dados requisitados ou realizar alguma outra ação.

## Aprofundando

É importante ressaltar que a autenticação básica no envio de solicitações HTTP não é considerada muito segura, pois as credenciais são transmitidas em texto simples e podem ser interceptadas por terceiros. Por isso, é aconselhável utilizar um protocolo HTTPS para criptografar a comunicação e torná-la mais segura.

Além disso, é importante validar as credenciais no servidor para garantir que apenas usuários autenticados tenham acesso às informações solicitadas.

## Veja também

- Documentação oficial do objeto XMLHttpRequest: https://developer.mozilla.org/pt-BR/docs/Web/API/XMLHttpRequest
- Tutorial sobre autenticação básica com Javascript: https://www.digitalocean.com/community/tutorials/js-xmlhttprequest-with-basic-authentication
- Vantagens e desvantagens da autenticação básica em requisições HTTP: https://www.owasp.org/index.php/Basic_authentication