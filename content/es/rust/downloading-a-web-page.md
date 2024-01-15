---
title:                "Descargando una página web"
html_title:           "Rust: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en el desarrollo web o simplemente quieres aprender un nuevo lenguaje de programación, descargar una página web puede ser una buena forma de tomar el primer paso. Puedes aprender cómo interactuar con una página web, extraer datos y utilizarlos en tus proyectos.

## Cómo hacerlo

La mejor forma de descargar una página web en Rust es utilizando la biblioteca `reqwest`. Primero, debes agregar esta biblioteca a tu archivo `Cargo.toml`:

```
[dependencies]
reqwest = { version = "0.11.4", features = ["json"] }
```

Luego, en tu archivo `.rs`, importa la biblioteca y utiliza su función `get` para enviar una solicitud al servidor de la página que deseas descargar:

```rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let response = reqwest::get("https://example.com").await?;
    println!("Status: {}", response.status());
    println!("Headers:\n{}", response.headers());
    println!("Body:\n{}", response.text().await?);
    Ok(())
}
```

La función `get` devuelve una estructura `Result`, así que es necesario manejar los posibles errores con `?` o `match`. En el ejemplo anterior, se imprime el estado de la respuesta, los encabezados y el cuerpo en formato de texto.

## Deep Dive

`reqwest` es una biblioteca completa para realizar solicitudes HTTP en Rust. Además de la función `get`, también puedes utilizar otras funciones como `post` o `delete` según tus necesidades. Puedes consultar la documentación oficial para obtener más información sobre las opciones de configuración y los métodos de solicitud disponibles.

Al utilizar `reqwest`, también puedes manejar cookies, autenticación, redirecciones y más. Además, usando la función `json` puedes analizar automáticamente la respuesta si está en formato JSON.

## Ver también

- [Documentación de `reqwest`](https://docs.rs/reqwest/0.11.4/reqwest/)
- [Tutorial de Rust para principiantes](https://www.rust-lang.org/es-ES/learn/get-started)
- [Proyectos de código abierto para contribuir en Rust](https://opensource.com/alternatives/rust-projects)