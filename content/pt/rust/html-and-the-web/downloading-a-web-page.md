---
date: 2024-01-20 17:44:41.657074-07:00
description: "Como fazer: Baixar p\xE1ginas da web \xE9 fundamental desde que a internet\
  \ entrou em uso. No Rust, a crate `reqwest` se destaca por oferecer uma interface\u2026"
lastmod: '2024-04-05T22:50:59.627179-06:00'
model: gpt-4-1106-preview
summary: "Baixar p\xE1ginas da web \xE9 fundamental desde que a internet entrou em\
  \ uso."
title: "Baixando uma p\xE1gina da web"
weight: 42
---

## Como fazer:
```Rust
// Adicione a crate reqwest ao seu Cargo.toml
// [dependencies]
// reqwest = "0.11"

use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    println!("Status: {}", res.status());
    println!("Headers:\n{:#?}", res.headers());
    let body = res.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```
Saída de exemplo:
```
Status: 200 OK
Headers:
{
    "content-type": "text/html; charset=UTF-8",
    ...
}
Body:
<!doctype html>
...
```

## Aprofundando
Baixar páginas da web é fundamental desde que a internet entrou em uso. No Rust, a crate `reqwest` se destaca por oferecer uma interface simples e segura. Há alternativas, como `hyper` (mais baixo nível) e `curl` (envolve ligação com C), mas `reqwest` é a escolha amigável. Internamente, `reqwest` usa `tokio` para assincronia e `hyper` para HTTP, mostrando como a comunidade Rust constrói abstrações poderosas sobre blocos de construção sólidos.

## Veja também:
- Documentação oficial `reqwest`: [docs.rs/reqwest](https://docs.rs/reqwest/)
- Guia do usuário `tokio`: [tokio.rs](https://tokio.rs/)
- Livro Oficial de Rust: [doc.rust-lang.org/book/](https://doc.rust-lang.org/book/)
