---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Rust: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Enviar uma solicitação HTTP com autenticação básica é um processo em que o usuário envia um pedido para um servidor web usando um padrão de segurança conhecido como autenticação básica. Isso é feito para garantir que a conexão entre o usuário e o servidor seja segura e para proteger informações confidenciais.

Os programadores usam esse método de autenticação principalmente quando há necessidade de autenticação em aplicativos da web, como sites de compras online e plataformas de redes sociais.

## Como fazer:

```Rust
use reqwest::blocking::Client;
use reqwest::header::HeaderValue;

fn main() -> Result<(), Box<std::error::Error>> {
    let client = Client::builder()
        .build()?;

    let mut headers = header::HeaderMap::new();

    headers.insert(
        header::AUTHORIZATION,
        HeaderValue::from_static("Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
    );

    let res = client
        .get("https://example.com")
        .headers(headers)
        .send()?;

    println!("Status: {:?}", res.status());
    Ok(())
}

```

Neste exemplo, usamos a biblioteca `reqwest` para enviar uma solicitação HTTP com autenticação básica. Primeiro, importamos a biblioteca e construímos um objeto `Client`. Em seguida, adicionamos o cabeçalho de autenticação básica ao objeto `HeaderMap` e enviamos o pedido usando a função `get` do cliente. Finalmente, imprimimos o status da resposta na saída.

## Deep Dive:

A autenticação básica foi criada na década de 1990 como um método simples de autenticação para conexões HTTP. No entanto, atualmente, é considerada uma forma fraca de autenticação, pois as informações de autenticação são enviadas como texto simples. Portanto, é recomendável usar outros métodos de autenticação, como OAuth ou autenticação de token.

Além disso, existem diferentes maneiras de implementar a autenticação básica em Rust, como usando a biblioteca `hyper` ou implementando manualmente o algoritmo de base64 para codificar a chave de autenticação.

## Veja também:

- Documentação oficial do Rust sobre autenticação básica: https://doc.rust-lang.org/std/net/trait.ToSocketAddrs.html
- Tutorial sobre autenticação básica em Rust: https://www.ameyalokare.com/rust/2018/09/20/rust-and-http-basic-authentication.html