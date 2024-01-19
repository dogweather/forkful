---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/parsing-html.md"
---

{{< edit_this_page >}}

# Entendiendo y utilizando HTML Parsing en Rust

## ¿Qué y Por Qué?

El análisis de HTML (HTML Parsing) es la traducción de código HTML a una representación más manejable, como un árbol de nodos - una necesidad comúnmente surge al desarrollar web crawlers, scrapers, o al modificar el contenido web existente.

## ¿Cómo se hace?

Para ilustrar, utilizaremos `html5ever`, una biblioteca de Rust para análisis de HTML de alta velocidad. Primero, instale la dependencia agregándola a su `Cargo.toml`.

```Rust
[dependencies]
html5ever = "0.25.1"
```

Ahora, podemos construir un simple análisis de HTML utilizando `html5ever`.

```Rust
extern crate html5ever;

use html5ever::parse_document;
use html5ever::rcdom::RcDom;
use html5ever::tendril::TendrilSink;

let html_content = "<html><body><h1>Hola Mundo!</h1></body></html>";

let dom = parse_document(RcDom::default(), Default::default())
  .from_utf8()
  .read_from(&mut html_content.as_bytes())
  .unwrap();

println!("{:#?}", dom.document);
```

La salida será la representación del árbol de nodos de su entrada HTML.

## Profundización

El análisis de HTML se remonta a los primeros días de la web. Durante mucho tiempo, los detalles de implementación y la falta de estándares llevaron a parseadores torpes y propensos a errores. Con la llegada de HTML5, se adoptaron estándares más estrictos para el parsing de HTML, permitiendo bibliotecas como `html5ever`.

Hoy en día, hay alternativas a `html5ever`. `scraper` es otra biblioteca de Rust que proporciona una interfaz de alto nivel para analizar HTML y manipularlo.

Lo interesante de `html5ever` es que tiene como objetivo estar completamente en conformidad con la [especificación de parsing de HTML5](https://html.spec.whatwg.org/multipage/parsing.html), lo que significa que funciona de la misma manera que los navegadores web modernos.

## Ver También

- Documentación oficial de `html5ever`: https://docs.rs/html5ever/
- Especificación de parsing de HTML5: https://html.spec.whatwg.org/multipage/parsing.html
- `scraper`, una alternativa a `html5ever`: https://docs.rs/scraper/