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

## Por que

Você já se deparou com a necessidade de se comunicar com outros sistemas ou serviços pela internet? As requisições HTTP são uma forma essencial de troca de informações nos dias de hoje. Aprenda como enviar uma requisição usando o Gleam e expanda suas capacidades de desenvolvimento web.

## Como fazer

```Gleam
// Importar o módulo HTTP
import gleam/http

// Definir a URL do serviço que você deseja se comunicar
let url = "https://example.com"

// Criar um objeto de requisição HTTP com o método GET e a URL
let request = http.request.get(url)

// Enviar a requisição e obter a resposta
let response = request.send()

// Exibir o código de status da resposta
gleam/io.format("Status code: {}", [response.status_code])

// Exibir o corpo da resposta
gleam/io.println(response.body)
```

O código acima mostra como enviar uma requisição GET usando o módulo HTTP do Gleam. Agora, vamos ver como seria para enviar uma requisição POST com corpo e cabeçalhos personalizados:

```Gleam
// Importar o módulo HTTP
import gleam/http

// Criar um objeto de requisição HTTP com o método POST e a URL
let request = http.request.post(url)

// Definir o corpo da requisição
let body = "{\"username\": \"john\", \"password\": \"securepassword\"}"

// Definir os cabeçalhos da requisição
let headers = [("Content-Type", "application/json"), ("Authorization", "Bearer abcdefgh")]

// Definir a requisição com o corpo e cabeçalhos
let request = request
  |> http.request.set_body(body)
  |> http.request.set_headers(headers)

// Enviar a requisição e obter a resposta
let response = request.send()

// Exibir o código de status da resposta
gleam/io.format("Status code: {}", [response.status_code])

// Exibir o corpo da resposta
gleam/io.println(response.body)
```

Note que o corpo e os cabeçalhos da requisição são definidos usando funções auxiliares `http.request.set_body` e `http.request.set_headers`. Essas funções permitem que você personalize sua requisição de acordo com suas necessidades.

## Mergulho profundo

O módulo HTTP do Gleam fornece inúmeras funções e tipos para lidar com requisições e respostas HTTP. Além disso, ele também oferece suporte para downloads, uploads e autenticação HTTP básica e digest.

Para saber mais sobre todas as funcionalidades disponíveis, confira a documentação oficial do módulo HTTP do Gleam [aqui](https://hexdocs.pm/gleam/http.html).

## Veja também

-[Documentação do módulo HTTP do Gleam](https://hexdocs.pm/gleam/http.html)
-[Tutorial de como enviar uma requisição HTTP usando o Gleam](https://dev.to/radibr/using-http-module-to-send-requests-on-gleam-2d76)
-[Exemplos de código do Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)