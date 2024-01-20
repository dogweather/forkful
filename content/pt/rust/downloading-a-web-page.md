---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Fazer o download de uma página web significa resgatar todos os seus dados e salvá-los localmente. Programadores fazem isso para analisar ou manipular o conteúdo de uma página web sem depender da conexão com a internet.

## Como Fazer:

O exemplo em Rust abaixo usa a biblioteca popular `reqwest` para baixar uma página web:

```Rust
use reqwest::Error;

async fn download() -> Result<(), Error> {
    let res = reqwest::get("https://www.rust-lang.org").await?;
    let body = res.text().await?;
    println!("corpo da página: {}", body);
    Ok(())
}

fn main() -> Result<(), Error> {
    let downloaded = download();
    Ok(())
}
```
O código primeiro solicita a página (linha 4), extrai o texto (linha 5) e imprime os conteúdos (linha 6).

## Mergulho Profundo

Desde os primórdios da web, a necessidade de baixar páginas web tem sido um aspecto importante da programação. Todavia, Rust moderniza essa tarefa. Antes, as bibliotecas comuns em C, por exemplo, eram menos seguras e mais propensas a erros. 

Existem alternativas à `reqwest` em Rust, como a `hyper` e a `surf`. Cada uma tem suas próprias vantagens dependendo do caso de uso. 

Sobre a implementação, `reqwest` é construída sobre `hyper`, uma biblioteca HTTP de baixo nível, combinando a velocidade de `hyper` com uma API fácil de usar. A função `get` faz um pedido HTTP GET para a URL fornecida. O método `text()` lê todo o corpo da resposta e o retorna como uma string.

## Ver Também

1. Documentação `reqwest`: [https://docs.rs/reqwest/](https://docs.rs/reqwest/)
2. Biblioteca `hyper`: [https://hyper.rs/](https://hyper.rs/)
3. Biblioteca `surf`: [https://docs.rs/surf/](https://docs.rs/surf/)
4. Tutoriais Rust: [https://www.rust-lang.org/pt-BR/tools/install](https://www.rust-lang.org/pt-BR/tools/install)