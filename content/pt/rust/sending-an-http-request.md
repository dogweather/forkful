---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviando uma requisição HTTP com Rust

## O que & Por que?

Enviar uma requisição HTTP é a forma de comunicar entre um cliente (seu programa) e um servidor web. Programadores fazem isso para interagir com serviços na web como APIs, baixar conteúdo e muito mais.

## Como fazer:

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://httpbin.org/get").await?;
    println!("Status: {}", response.status());
    let body = response.text().await?;
    println!("Body:\n\n{}", body);
    Ok(())
}
```

Quando você executa esse código, você receberá uma resposta semelhante a essa:

```Rust
Status: 200 OK
Body:

{
  "args": {}, 
  "headers": {
    ...
  }, 
  ...
}
```

## Aprofundando

Enviar requisições HTTP tem sido uma atividade fundamental em programação web desde os primeiros dias da internet. No contexto de Rust, existem várias bibliotecas que facilitam essa ação, como `reqwest`, `hyper`, e `ureq`.

Embora nesse tutorial tenhamos usado a biblioteca `reqwest`, você poderia considerar `hyper` se precisasse de um maior controle sobre os aspectos de baixo nível das requisições HTTP, ou `ureq` para uma abordagem mais minimalista e síncrona.

Por baixo dos panos, estas bibliotecas estão lidando com a geração de uma requisição HTTP correta, o envio desta requisição ao servidor, a análise da resposta do servidor e a entrega desta resposta ao resto do seu código.

## Veja também

Para mais detalhes sobre como fazer requisições HTTP com Rust, confira estes links:

- [Documentação do Reqwest](https://docs.rs/reqwest)
- [Uso do Ureq](https://github.com/algesten/ureq)