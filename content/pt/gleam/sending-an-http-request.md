---
title:                "Enviando uma solicitação http"
html_title:           "Gleam: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Enviar uma solicitação HTTP é uma forma de comunicar-se com outros servidores através da internet. Os programadores fazem isso para obter ou enviar informações, acessar APIs ou realizar outras ações em sistemas remotos.

## Como fazer:
```
Gleam.http.send("https://api.exemplo.com/users", { method: "GET" })
// => Resposta: Lista de usuários
```
```
Gleam.http.send("https://api.exemplo2.com/users", { method: "POST", body: "{ name: 'Maria', age: 25 }" })
// => Resposta: Usuário criado com sucesso
```

## Profundidade:
Enviar solicitações HTTP é um recurso essencial para a comunicação na internet desde o surgimento da World Wide Web em 1989. Existem várias alternativas para enviar essas solicitaçõesem outras linguagens de programação, como cURL em PHP ou request em Node.js. No entanto, com a sintaxe simples e limpa do Gleam, é possível enviar solicitações HTTP de forma rápida e fácil.

## Veja também:
- [Documentação oficial do Gleam sobre HTTP](https://gleam.run/documentation/library/http/)
- [Exemplo de uso de HTTP no Gleam](https://github.com/gleam-lang/gleam/blob/main/examples/http_backup/backup.gleam)