---
title:                "Gleam: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Enviar uma solicitação HTTP com autenticação básica é essencial para acessar recursos protegidos em um servidor. Sem a autenticação adequada, esses recursos não serão acessíveis, tornando a comunicação com o servidor impossível.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Gleam, você precisará ter em mãos o URL do servidor e as credenciais de autenticação (nome de usuário e senha). Em seguida, você pode usar a função `httpc.request` e passar os parâmetros necessários, como headers e corpo da requisição. Veja um exemplo de código abaixo:

```Gleam

// Importe o módulo httpc
import httpc

// Defina as credenciais
let username = "seu_usuario"
let password = "sua_senha"

// Defina o URL do servidor
let url = "https://exemplo.com"

// Configure os headers e o corpo da requisição
let headers = [("Content-Type", "application/json")]
let body = "{}"

// Envie a solicitação com autenticação básica
let response = httpc.request(
  "POST",
  url,
  headers,
  body,
  username,
  password
)

// Imprima a resposta com código de status e corpo
case response {
  Ok(response) ->
    let status_code = response.status_code
    let body = response.body
    IO.println("Código de status: #{status_code}")
    IO.println("Corpo: #{body}")
  Err(err) -> IO.println("Erro: #{err}")
}
```

A saída do código acima seria algo como:

```
Código de status: 200
Corpo: {"mensagem": "Requisição bem sucedida"}
```

## Deep Dive

Ao enviar uma solicitação HTTP com autenticação básica, o cabeçalho da requisição deve conter o parâmetro `Authorization`. Ele terá o valor `Basic`, seguido pelo nome de usuário e senha codificados em Base64. Além disso, é importante lembrar que esse tipo de autenticação não é considerado seguro, pois as credenciais são enviadas como texto simples. É recomendável utilizar SSL/TLS em conjunto com a autenticação básica para garantir a segurança dos dados.

## Veja também

- Documentação oficial do módulo httpc em Gleam: https://github.com/gleam-lang/httpc
- Tutorial sobre autenticação básica em HTTP: https://www.digitalocean.com/community/tutorials/how-to-use-basic-authentication-with-http-in-gleam