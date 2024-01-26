---
title:                "Enviando uma requisição HTTP"
date:                  2024-01-20T17:59:34.753719-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma solicitação HTTP é o processo de pedir informações ou enviar dados para um servidor. Programadores fazem isso para interagir com a web, acessar APIs, e trocar dados através de redes.

## Como Fazer:
```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() {
  case httpc.send(http.Request(method: Get, url: "http://example.com")) {
    Ok(response) ->
      io.println("Recebido: {response.body}")
    
    Error(error) ->
      io.println("Erro ao enviar a solicitação: {error}")
  }
}
```
Sample output:
```
Recebido: <html>...</html>
```

## Mergulho Profundo
Historicamente, enviar requisições HTTP era mais complexo e dependia de várias bibliotecas. Gleam simplificou esse processo, fornecendo uma sintaxe mais limpa e expressiva. Alternativas como Curl e bibliotecas de outras linguagens existem, mas Gleam preza pela tipagem forte e segurança de concorrência. Ao mandar uma requisição HTTP em Gleam, você se beneficia de um sistema de tipos que previne erros comuns em tempo de compilação.

## Veja Também
- Documentação oficial do Gleam HTTP: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- RFC 7231, para a especificação de HTTP/1.1: [https://tools.ietf.org/html/rfc7231](https://tools.ietf.org/html/rfc7231)
