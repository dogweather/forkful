---
title:                "Analisando HTML"
aliases:
- /pt/rust/parsing-html.md
date:                  2024-02-03T19:12:57.660825-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar HTML em Rust trata-se de extrair dados de documentos HTML, o que é essencial para web scraping, extração de dados ou construção de web crawlers. Programadores fazem isso para automatizar a coleta de informações da web, analisar conteúdo da web ou migrar conteúdo de uma plataforma para outra.

## Como fazer:

Para analisar HTML em Rust, você normalmente usará o crate `scraper`, que fornece uma interface de alto nível para percorrer e manipular documentos HTML.

Primeiro, adicione `scraper` ao seu `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

A seguir, um exemplo simples que extrai todos os URLs de links de uma string HTML dada:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Link 1</a>
        <a href="http://example.com/2">Link 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("Link encontrado: {}", link);
    }
}
```

Saída:

```
Link encontrado: http://example.com/1
Link encontrado: http://example.com/2
```

Neste exemplo, analisamos um documento HTML simples para encontrar todos os elementos `<a>` e extrair seus atributos `href`, efetivamente imprimindo os URLs de todos os links no documento. A biblioteca `scraper` simplifica a análise de HTML e a seleção de elementos específicos usando seletores CSS, tornando-a uma escolha popular para tarefas de web scraping em Rust.
