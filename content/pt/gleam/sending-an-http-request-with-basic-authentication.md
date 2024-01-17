---
title:                "Enviar uma solicitação http com autenticação básica"
html_title:           "Gleam: Enviar uma solicitação http com autenticação básica"
simple_title:         "Enviar uma solicitação http com autenticação básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Enviar uma solicitação HTTP com autenticação básica é uma forma de garantir que o servidor que está sendo acessado é autêntico e que a comunicação é protegida de acesso não autorizado. Os programadores fazem isso para garantir que suas aplicações estejam seguras e que apenas usuários autorizados tenham acesso aos recursos protegidos no servidor.

## Como fazer:

```
Gleam.http.send_with_basic_auth("https://meuservidor.com", "usuário", "senha")
```

Saída:
```
{ok, "200 OK"}
```

## Profundando:

Enviar uma solicitação HTTP com autenticação básica é uma prática comum em programação, pois garante que a comunicação entre um cliente e um servidor seja segura e autenticada. Existem outras formas de autenticação, como OAuth ou token de acesso, mas a autenticação básica é uma das mais simples de ser implementada. Além disso, é amplamente suportada por servidores e clientes web.

## Veja também:

- Documentação oficial do Gleam para solicitações HTTP: https://gleam.run/modules/http.html
- Como usar tokens de acesso em vez de autenticação básica: https://gleam.run/blog/basic-auth-and-tokens.html