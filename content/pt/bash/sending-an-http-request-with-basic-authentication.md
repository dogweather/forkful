---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando uma solicitação HTTP com autenticação básica em Bash

## O quê & por quê?

Enviar uma solicitação HTTP com autenticação básica é uma forma de interagir com um servidor web que requer um nome de usuário e senha. Programadores fazem isso para acessar serviços protegidos por senha ou para criar, recuperar, atualizar e excluir dados via API.

## Como fazer:

Para enviar uma solicitação GET com autenticação básica, usamos o comando `curl` assim:

```Bash
usuario='meu_usuario'
senha='minha_senha'
url='http://meuservidor.com/api'

curl -u $usuario:$senha $url
```

A saída será a resposta do servidor para a sua solicitação.

## Mergulho profundo

1. **Contexto histórico**: A autenticação básica é um método antigo para proteger áreas da web, introduzido pela primeira vez em 1996 como parte do protocolo HTTP 1.0.

2. **Alternativas**: Enquanto a autenticação básica é simples, ela tem desvantagens. Uma alternativa comum é a autenticação de token, usando o JWT (JSON Web Token) ou OAuth.

3. **Detalhes de implementação**: Em 'curl -u $usuario:$senha $url', a opção `-u` refere-se a "user" que informa ao cURL que você deseja usar a autenticação básica. A sequência `$usuario:$senha` é convertida em uma string codificada em base64 que é passada no cabeçalho da solicitação.

## Veja também

1. [Curl man page](https://curl.haxx.se/docs/manpage.html)
2. [Guia HTTP/1.1 da Mozilla](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Headers/Authorization)
3. [JWT](https://jwt.io/introduction/)
4. [OAuth](https://oauth.net/)