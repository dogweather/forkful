---
title:                "Fazendo o download de uma página da web"
html_title:           "Rust: Fazendo o download de uma página da web"
simple_title:         "Fazendo o download de uma página da web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que baixar uma página da web?

Há várias razões pelas quais alguém pode querer baixar uma página da web. Pode ser para fazer backup de conteúdo importante, para acessar o conteúdo offline ou para analisar o código fonte da página para fins de estudo ou aprimoramento de suas próprias habilidades de programação.

## Como fazer

Para baixar uma página da web em Rust, você pode usar a biblioteca `reqwest`, que é amplamente utilizada pela comunidade de programação Rust para fazer solicitações HTTP. Primeiro, adicione `reqwest` como dependência ao seu projeto em `Cargo.toml`:

```Rust
[dependencies]
reqwest = { version = "0.11", features = ["json"] }
```

Em seguida, importe a biblioteca no seu código:

```Rust
use reqwest::blocking::get;
```

Agora, podemos usar a função `get()` para fazer uma solicitação HTTP e salvar o conteúdo da resposta em uma variável. Por exemplo, para baixar a página inicial da Wikipedia em inglês, podemos fazer o seguinte:

```Rust
let response = get("https://en.wikipedia.org/wiki/Main_Page")?;
let body = response.text()?;
```

O `?` no final de cada linha é usado para lidar com possíveis erros que podem ocorrer durante a execução. Isso é conhecido como "try operator" e é usado para evitar o uso excessivo de `match` ou `unwrap`, tornando o código mais legível.

O conteúdo da página será salvo na variável `body`, que podemos, então, imprimir na tela ou salvar em um arquivo.

## Profundidade

A biblioteca `reqwest` também oferece várias opções de configuração, como adicionar cabeçalhos personalizados ou lidar com redirecionamentos. Além disso, é possível fazer solicitações assíncronas usando `reqwest::Client`. Você também pode usar outras bibliotecas, como `scraper`, para analisar o conteúdo HTML da página baixada.

## Veja também

- [Documentação da biblioteca `reqwest`](https://docs.rs/reqwest)
- [Exemplo de código para baixar uma página da web em Rust](https://github.com/catak1st/reqwest-example-rs)