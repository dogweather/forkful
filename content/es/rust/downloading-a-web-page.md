---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Descargar una página web simplemente significa recuperar su contenido del servidor y almacenarlo localmente. Los programadores hacen esto por muchas razones, desde reunir datos para el análisis hasta probar la funcionalidad de una aplicación.

## Cómo se Hace:

Aquí hay un ejemplo de cómo podemos descargar una página web en Rust utilizando la biblioteca "reqwest". 

```Rust 
use reqwest;
use std::fs::write;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let content = reqwest::get("https://www.example.com")
        .await?
        .text()
        .await?;

    write("output.html", content).expect("Unable to write file");

    println!("La página web se ha descargado correctamente.");

    Ok(())
}   
```
Al ejecutarlo, si todo va bien, verás en la terminal:
 
``` La página web se ha descargado correctamente. ``` 

## Profundización

Históricamente, los programadores tenían que utilizar lenguajes de bajo nivel para recuperar datos de la web, lo que a menudo era una tarea tediosa y propensa a errores. Rust, sin embargo, gracias a sus robustas bibliotecas como `reqwest`, hace que este proceso sea increíblemente simple.

Existen alternativas a `reqwest` como `hyper`, que proporciona una interfaz de bajo nivel, y `surf`, que es asincrónico por diseño. Sin embargo, `reqwest` es excelente por su facilidad de uso y su amplio conjunto de características.

Detrás de escena, cuando descargamos una página web, una solicitud HTTP se envía al servidor que aloja la página. El servidor responde con el contenido de la página (generalmente en HTML), que luego almacenamos en un archivo local o usamos en nuestra aplicación.

## Ver También

- [reqwest en docs.rs](https://docs.rs/reqwest/0.10.8/reqwest/)
- [Guía de programación en Rust](https://doc.rust-lang.org/book/title-page.html)
- [hyper en docs.rs](https://docs.rs/hyper/0.14.7/hyper/)
- [surf en docs.rs](https://docs.rs/surf/2.2.2/surf/)