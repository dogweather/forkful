---
title:                "Analizando HTML"
aliases:
- es/rust/parsing-html.md
date:                  2024-02-03T19:12:57.663092-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analizando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Analizar HTML en Rust se trata de extraer datos de documentos HTML, lo cual es esencial para el web scraping, la extracción de datos o la construcción de recolectores web. Los programadores hacen esto para automatizar la recopilación de información de la web, analizar el contenido web o migrar contenido de una plataforma a otra.

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
