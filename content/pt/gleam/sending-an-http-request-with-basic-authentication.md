---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Enviar um pedido HTTP com autenticação básica permite que os programadores solicitem dados de uma API de servidor remoto, mantendo as informações de autenticação dos usuários seguras. Isso é crucial para manter a integridade das contas do usuário e evitar acessos não autorizados.

## Como Fazer:

```Gleam
import gleam/http.@{get, RequestBuilder}
import gleam/string

fn make_request() {
  let url = Uri.parse("http://api.exemplo.com/dados")

  url
  |> http.get()
  |> http.prepend_to_header("Authorization", "Basic " <> string.base64_encode("usuario:senha"))
  |> http.send()
}
```

O código acima gera e envia uma solicitação GET HTTP para "http://api.exemplo.com/dados". A linha 9 aqui adiciona um cabeçalho de Autorização com os detalhes do usuário codificados em Base64.

## Mergulho Profundo:

Historicamente, a autenticação básica HTTP é usada há bastante tempo na programação web devido à sua simplicidade. No entanto, é importante notar que, sem uma conexão HTTPS segura, as informações de autenticação base64 podem ser decodificadas facilmente.

Existem várias alternativas à autenticação básica HTTP, incluindo a autenticação baseada em token e a autenticação OAuth. Esses métodos oferecem maior segurança, mas também são mais complexos para implementar.

Em Gleam, estamos usando o módulo `http` para construir nossa solicitação HTTP e enviar. Isso mantém nossa implementação limpa e simples, sem a necessidade de manipular detalhes de baixo nível como a criação da conexão de rede.

## Veja Também:

Para mais informações sobre a autenticação HTTP e outras opções de segurança do Gleam, você pode achar útil as seguintes leituras:

- [Documentação Gleam HTTP](https://hexdocs.pm/gleam_http/readme.html)
- [Tutorial de Autenticação HTTP da MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Artigo sobre Autenticação Base64](https://www.base64decode.net/base64-overview)