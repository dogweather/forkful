---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Gleam: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que
Aprender a enviar uma solicitação HTTP com autenticação básica é importante em uma variedade de casos, como acessar APIs de terceiros, autenticar usuários em um aplicativo ou receber dados de uma fonte segura.

## Como fazer
Para enviar uma solicitação HTTP com autenticação básica em Gleam, você precisará de duas coisas: uma biblioteca HTTP e as credenciais de autenticação. Vamos usar a biblioteca [Httpc](https://github.com/gleam-lang/httpc) neste exemplo.

Primeiro, importe a biblioteca em seu módulo:

```Gleam
import httpc
```

Em seguida, defina as credenciais de autenticação em uma variável:

```Gleam
let credentials = httpc.BasicAuthCredentials("username", "password")
```

Agora, podemos enviar uma solicitação HTTP usando as funções fornecidas pela biblioteca Httpc. Por exemplo, para fazer uma solicitação GET para um URL com autenticação básica, podemos usar a função `get_with_auth`:

```Gleam
let url = "https://exemplo.com/recurso"
let response = httpc.get_with_auth(url, credentials)
```

O objeto de resposta fornecido pela função contém o corpo da resposta e outras informações, como o código de status e os cabeçalhos. Você pode acessar esses dados para manipulá-los em seu aplicativo.

## Mergulho profundo
A autenticação básica é um método simples de autenticação que envolve enviar as credenciais (nome de usuário e senha) no cabeçalho da solicitação HTTP. Essas credenciais são codificadas em Base64 para proteger contra acessos não autorizados.

É importante notar que a autenticação básica não é considerada um método de segurança robusto e pode ser facilmente interceptada e lida por hackers. Portanto, é recomendável usar outros métodos de autenticação, como OAuth, para garantir a segurança dos dados.

## Veja também
- Biblioteca Httpc: https://github.com/gleam-lang/httpc
- Documentação oficial de autenticação básica: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#basic_authentication_scheme