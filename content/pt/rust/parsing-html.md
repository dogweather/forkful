---
title:                "Análise de HTML"
date:                  2024-01-20T15:33:57.910574-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"

category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é e Por Quê?
Analisar HTML significa dissecar o código de uma página web para entender sua estrutura e conteúdo. Programadores fazem isso para extrair dados, manipular o conteúdo ou migrar informações para outros formatos.

## Como Fazer:
Vamos ver o básico com a crate `scraper`. Primeiro, adicione no `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

Depois, um exemplo de código em Rust:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html_content = r#"
        <html>
            <body>
                <h1>Olá, Rustaceans!</h1>
                <p>Scraping é divertido com Rust.</p>
            </body>
        </html>
    "#;

    let document = Html::parse_document(html_content);
    let selector = Selector::parse("h1").unwrap();
    let h1 = document.select(&selector).next().unwrap().inner_html();

    println!("Conteúdo encontrado: {}", h1);
}
```

Resultado:

```
Conteúdo encontrado: Olá, Rustaceans!
```

## Mergulho Profundo:

**Contexto Histórico**: HTML é a linguagem de marcação fundamental da web desde os anos 90. Extrair dados de páginas HTML é uma necessidade que surgiu com a própria web.

**Alternativas**: Existem várias bibliotecas para analisar HTML em muitas linguagens. Em Rust, além do `scraper`, você pode usar `html5ever` ou `select.rs`.

**Detalhes de Implementação**: `scraper` é construído sobre a crate `html5ever`, que faz o parsing compatível com o padrão HTML5. Isso garante uma análise precisa mesmo com HTML "sujo" ou malformado.

## Veja Também:

- Documentação da crate `scraper`: https://docs.rs/scraper/latest/scraper/
- Livro "The Rust Programming Language": https://doc.rust-lang.org/book/
- Tutorial de Web Scraping com Rust: https://www.freecodecamp.org/news/how-to-build-a-web-scraper-with-rust/
