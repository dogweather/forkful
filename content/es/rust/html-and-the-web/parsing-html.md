---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:57.663092-07:00
description: "C\xF3mo: Para analizar HTML en Rust, a menudo utilizar\xE1s el crate\
  \ `scraper`, que proporciona una interfaz de alto nivel para recorrer y manipular\
  \ documentos\u2026"
lastmod: '2024-03-13T22:44:58.844313-06:00'
model: gpt-4-0125-preview
summary: "Para analizar HTML en Rust, a menudo utilizar\xE1s el crate `scraper`, que\
  \ proporciona una interfaz de alto nivel para recorrer y manipular documentos HTML."
title: Analizando HTML
weight: 43
---

## Cómo:
Para analizar HTML en Rust, a menudo utilizarás el crate `scraper`, que proporciona una interfaz de alto nivel para recorrer y manipular documentos HTML.

Primero, añade `scraper` a tu `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

A continuación, aquí tienes un ejemplo simple que extrae todas las URLs de enlaces de una cadena HTML dada:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Enlace 1</a>
        <a href="http://example.com/2">Enlace 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("Enlace encontrado: {}", link);
    }
}
```

Salida:

```
Enlace encontrado: http://example.com/1
Enlace encontrado: http://example.com/2
```

En este ejemplo, analizamos un documento HTML simple para encontrar todos los elementos `<a>` y extraer sus atributos `href`, imprimiendo efectivamente las URLs de todos los enlaces en el documento. La biblioteca `scraper` simplifica el análisis y la selección de elementos HTML específicos utilizando selectores CSS, convirtiéndola en una opción predilecta para tareas de web scraping en Rust.
