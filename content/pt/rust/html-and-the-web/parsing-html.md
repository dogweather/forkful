---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:57.660825-07:00
description: "Como fazer: Para analisar HTML em Rust, voc\xEA normalmente usar\xE1\
  \ o crate `scraper`, que fornece uma interface de alto n\xEDvel para percorrer e\
  \ manipular\u2026"
lastmod: '2024-03-13T22:44:46.365570-06:00'
model: gpt-4-0125-preview
summary: "Para analisar HTML em Rust, voc\xEA normalmente usar\xE1 o crate `scraper`,\
  \ que fornece uma interface de alto n\xEDvel para percorrer e manipular documentos\
  \ HTML."
title: Analisando HTML
weight: 43
---

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
