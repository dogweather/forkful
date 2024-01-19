---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando uma requisição HTTP com autenticação básica em Rust 

## O que é e por quê?

Enviar uma requisição HTTP com autenticação básica é uma maneira de garantir que a informação que você está compartilhando seja feita de maneira segura. O objetivo é restringir o acesso a recursos de um servidor HTTP a usuários autorizados.

## Como fazer:

Vamos usar a biblioteca `reqwest` para enviar requisições HTTP.

```Rust
use reqwest::header::{HeaderMap, HeaderValue, AUTHORIZATION, USER_AGENT};
use base64::encode;

// Definindo a URL
let url = "https://api.seusite.com/endpoint";

// Definindo as credenciais para autenticação básica
let user = "usuario";
let pass = "senha";

// Codificando as credenciais
let encoded_auth = encode(&format!("{}:{}", user, pass));

// Criando o HeaderMap
let mut headers = HeaderMap::new();
headers.insert(USER_AGENT, HeaderValue::from_static("Reqwest"));
headers.insert(AUTHORIZATION, HeaderValue::from_str(&format!("Basic {}", encoded_auth)).unwrap());

// Enviando a requisição HTTP com autenticação básica
let client = reqwest::Client::builder()
    .default_headers(headers)
    .build()
    .unwrap();

let response = client.get(url).send().await;

match response {
    Ok(_) => println!("Solicitação bem-sucedida!"),
    Err(e) => println!("Ocorreu um erro: {}", e),
}
```

## Deep Dive

A autenticação básica é uma das maneiras mais simples de controlar o acesso aos recursos de um servidor desde os primórdios da web. Não é a mais segura ou sofisticada, mas em certos cenários, pode ser suficiente e mais fácil de implementar.

Há alternativas como a autenticação Digest, que é mais segura e um pouco mais complexa. A autenticação de token Bearer (ou JWT) também é outra alternativa comum em APIs REST.

A implementação de autenticação básica no Rust usando `reqwest` envolve a criação de headers de autorização que contêm as credenciais do usuário codificadas em base64. Observação: a codificação base64 não é uma forma de criptografia. Ela apenas mascara os dados reais para que não sejam lidos diretamente.

## Veja também

- Documentação `reqwest`: https://docs.rs/reqwest/
- RFC 7617 (Autenticação Básica): https://tools.ietf.org/html/rfc7617
- Autenticação Digest: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#digest_authentication
- JSON Web Tokens (JWT): https://jwt.io/