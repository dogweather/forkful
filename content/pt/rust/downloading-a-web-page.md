---
title:                "Baixando uma página da web"
html_title:           "Rust: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e porque?

Download de uma página da web é o ato de baixar todo o código HTML e seus recursos associados (CSS, JavaScript, imagens, etc.) de uma página da web. Programadores geralmente fazem isso para analisar o código fonte de uma página da web, para automatizar tarefas como a coleta de dados ou para criar scripts personalizados para navegadores.

## Como fazer:

```Rust
use reqwest; // importando a biblioteca 'reqwest' para fazer a requisição HTTP

let response = reqwest::get("https://www.example.com").await?; // realizando a requisição e guardando a resposta em uma variável

let body = response.text().await?; // obtendo o código fonte da página da web

println!("{}", body); // imprimindo o código fonte no console
```

**Saída:** ```<!DOCTYPE html> <html> <head> ... </head> <body> ... </body> </html> ```

## Mergulho profundo:

O download de uma página da web tem sido uma prática muito comum desde o início da internet. Antigamente, os programadores usavam bibliotecas como wget e cURL para fazer esse tipo de requisição. Atualmente, existem muitas bibliotecas em diversas linguagens de programação, incluindo o Rust, que tornam o processo mais simples e eficiente.

Existem também outras alternativas para fazer download de páginas da web, como o uso de APIs ou a raspagem de dados por meio de técnicas de web scrapping. No entanto, fazer uma requisição HTTP continua sendo uma das formas mais diretas e efetivas de obter o código fonte de uma página.

No caso do Rust, a biblioteca mais comumente utilizada para fazer download de páginas da web é a `reqwest`. Ela permite fazer requisições síncronas e assíncronas, possui uma interface simples e é altamente configurável.

## Veja também:

- [Documentação da biblioteca reqwest](https://docs.rs/reqwest)
- [Exemplo de script em Rust para baixar páginas da web](https://gist.github.com/nikomatsakis/94c29cb25df71d4a4c5694b30b775bc1)
- [Outras opções para fazer download de páginas da web em Rust](https://rustrepo.com/catalog/rust-network-programming-downloader)