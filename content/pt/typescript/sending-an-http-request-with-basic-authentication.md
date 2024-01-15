---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "TypeScript: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Enviar uma solicitação HTTP com autenticação básica é essencial para acessar recursos protegidos em um servidor. Isso garante a segurança da comunicação com o servidor e permite que o mesmo verifique se o usuário é autorizado a acessar determinados dados ou realizar determinadas ações.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em TypeScript, você vai precisar de uma biblioteca de cliente HTTP, como o "Axios". Primeiro, importe a biblioteca no seu arquivo TypeScript:

```TypeScript
import axios from 'axios';
```

Em seguida, crie um objeto de configuração que contenha o endpoint da solicitação, as credenciais de autenticação e outros parâmetros opcionais, como headers ou dados a serem enviados:

```TypeScript
let config = {
    url: 'https://api.example.com/resource',
    auth: {
        username: 'usuario',
        password: 'senha'
    },
    headers: {
        'Content-Type': 'application/json'
    },
    data: {
        foo: 'bar'
    }
};
```

Por fim, use o método "axios.request()" para enviar a solicitação com a configuração criada:

```TypeScript
axios.request(config)
    .then(response => console.log(response.data))
    .catch(error => console.log(error));
```

## Deep Dive

A autenticação básica é um método simples de autenticação baseado em criptografia de texto simples. Ao enviar uma solicitação com autenticação básica, o cliente inclui as credenciais do usuário (nome de usuário e senha) criptografadas no cabeçalho da solicitação. O servidor, por sua vez, verifica se as credenciais correspondem a um usuário autorizado e retorna os dados solicitados se a autenticação for bem-sucedida.

É importante lembrar que a autenticação básica não é considerada um método seguro, pois as credenciais são enviadas em texto simples e podem ser interceptadas por terceiros. Portanto, é recomendável utilizar métodos de autenticação mais robustos, como o OAuth, sempre que possível.

## Veja também

- Documentação oficial do Axios: https://github.com/axios/axios
- Tutorial sobre autenticação básica em HTTP: https://www.baeldung.com/spring-security-basic-authentication
- Artigo sobre segurança de dados em comunicações HTTP: https://owasp.org/www-community/controls/Authentication