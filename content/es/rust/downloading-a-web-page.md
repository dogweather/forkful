---
title:                "Descargando una página web"
date:                  2024-01-20T17:44:51.299002-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Descargar una página web es obtener el contenido HTML de una URL específica. Los programadores hacen esto para analizar datos, probar aplicaciones y automatizar tareas en la web.

## Cómo hacerlo:

Para descargar una página web en Rust, usarás la biblioteca `reqwest`. Instálala añadiendo a tu `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Aquí tienes un fragmento de código simple para hacer una solicitud GET y imprimir el resultado:

```rust
use reqwest;
use tokio;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "http://example.com";
    let res = reqwest::get(url).await?;

    let body = res.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```

Ejecuta tu código y deberías ver el HTML de "http://example.com" en la consola.

## Inmersión Profunda:

Antes de `reqwest`, opciones como `hyper` eran populares pero requerían más código y manejo manual de conexiones HTTP. `reqwest` es una abstracción de alto nivel que maneja estos detalles internamente. Para descargar una página web, podrías usar también `curl` en línea de comandos, pero en Rust, `reqwest` permite integrar la funcionalidad en tus aplicaciones.

Al trabajar con `reqwest`, es importante tener en cuenta detalles como el manejo de errores, el tiempo de espera de las solicitudes y las conexiones seguras (HTTPS). Al descargar contenido, una práctica recomendada es verificar el `status code` para asegurarse de que la solicitud fue exitosa.

Una alternativa a `reqwest` en el ecosistema de Rust es `surf`, aunque es menos popular y podría no tener todas las funcionalidades que necesitas.

## Ver También:

- Documentación de `reqwest`: https://docs.rs/reqwest/
- Libro Asíncrono de Rust: https://rust-lang.github.io/async-book/
- Guía de Tokio para tareas asíncronas: https://tokio.rs/tokio/tutorial
- Información sobre códigos de estado HTTP: https://developer.mozilla.org/es/docs/Web/HTTP/Status
