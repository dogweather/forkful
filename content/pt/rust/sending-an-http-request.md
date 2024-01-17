---
title:                "Enviando uma solicitação http"
html_title:           "Rust: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Enviar uma solicitação HTTP é um processo no qual um programa faz uma comunicação com o servidor por meio do protocolo HTTP. Programadores fazem isso para acessar e enviar dados entre o cliente e o servidor, permitindo a interação com aplicativos que funcionam na web.

## Como fazer:
```rust
use reqwest::blocking::{Client, Error};

fn main() -> Result<(), Error> {
    let client = Client::new();
    let response = client.get("https://www.example.com")
        .send()?;
    println!("{}", response.text()?);
    Ok(())
}
```

## Detalhando:
Enviar uma solicitação HTTP é uma ação comum em programação web. É uma maneira de fazer seu aplicativo se comunicar com outros servidores para acessar recursos da internet. Existem várias bibliotecas em Rust que podem ajudar nessa tarefa, como a popular reqwest.

## Veja também:
- [Documentação official do Rust sobre enviar solicitações HTTP](https://doc.rust-lang.org/stable/std/net/index.html)
- [Referência para a biblioteca reqwest em Rust](https://docs.rs/reqwest/0.11.1/reqwest/)
- [Exemplos de código para enviar solicitações HTTP em Rust](https://gist.github.com/nakabonne/f3fc181900d70457e88989dc5b783c9b)