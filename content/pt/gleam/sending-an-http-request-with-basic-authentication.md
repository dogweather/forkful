---
title:                "Enviando uma requisição HTTP com autenticação básica"
date:                  2024-01-20T18:01:51.365019-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar uma requisição HTTP com autenticação básica significa passar um usuário e senha para acessar um recurso protegido na web. Programadores fazem isso para interagir com APIs que requerem uma forma simples de identificação.

## Como Fazer:

```gleam
import gleam/http
import gleam/http/cowboy
import gleam/string

fn autentica_requisicao(req: http.Request(_)) {
  let base64_credenciais = string.base64_encode("usuario:senha")
  req
  |> http.request_header("Authorization", "Basic " ++ base64_credenciais)
}

pub fn exemplo_requisicao() {
  let req = http.default_request("https://exemplo.com/api")
            |> autentica_requisicao()

  case http.send(req) {
    Ok(resp) -> io.println("Sucesso: " ++ resp.body)
    Error(err) -> io.println("Erro ao enviar a requisição: " ++ error.to_string(err))
  }
}
```

_Saída de Amostra:_

```
Sucesso: {"dados": "Conteúdo Protegido Acessado"}
```

## Aprofundamento

Antes do HTTPS se tornar padrão, a autenticação básica HTTP era uma maneira direta de controlar o acesso. Hoje, com a preocupação crescente sobre segurança, ela é considerada menos segura se não for usada através de HTTPS, que criptografa as credenciais. Alternativas incluem tokens de acesso e autenticação OAuth, que fornecem métodos mais seguros e flexíveis para controle de acesso. A implementação em Gleam é direta e utiliza a codificação em Base64 das credenciais, necessária pela especificação da autenticação básica HTTP.

## Veja Também

- [MDN Web Docs on HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OWASP Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
