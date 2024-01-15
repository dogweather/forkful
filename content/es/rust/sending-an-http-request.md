---
title:                "Haciendo una solicitud http"
html_title:           "Rust: Haciendo una solicitud http"
simple_title:         "Haciendo una solicitud http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo funcionan las aplicaciones web? Bueno, detrás de cada clic en un enlace o botón en una página web, hay una solicitud HTTP que se envía al servidor. En este artículo, aprenderás cómo enviar una solicitud HTTP utilizando Rust y por qué es importante conocer este concepto.

## Cómo hacerlo

Para enviar una solicitud HTTP en Rust, primero debes incluir el paquete `reqwest` en tu `Cargo.toml`:

```
[dependencias]
reqwest = { versión = "0.11", características = ["tls"] }
```

A continuación, importa el paquete en tu código:

```
use reqwest;
```

Después de eso, puedes crear una función para enviar una solicitud GET a una URL determinada:

```
fn enviar_solicitud() -> Result<(), Box<std::error::Error>> {
    let respuesta = reqwest::get("https://jsonplaceholder.typicode.com/todos/1")?
        .text()?;

    println!("Respuesta: {}", respuesta);

    Ok(())
}
```

Como puedes ver, utilizamos el método `get()` del paquete `reqwest` y pasamos la URL a la que deseamos enviar la solicitud. Luego, usamos el método `text()` para obtener la respuesta como texto y la imprimimos en la consola.

Si ejecutas este código, deberías obtener la siguiente salida en la consola:

```
Respuesta: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive

Ahora, profundicemos un poco más en cómo funciona el envío de una solicitud HTTP en Rust. Primero, cuando se invoca el método `get()`, se crea una nueva instancia de `Client` (cliente) del paquete `reqwest` que mantiene una conexión HTTP activa. Este cliente guarda automáticamente una conexión TCP reutilizable para optimizar futuras solicitudes a la misma URL.

Luego, cuando llamamos al método `text()`, se inicia la conexión y se envía la solicitud GET al servidor. Una vez que se recibe una respuesta del servidor, se convierte a texto y se devuelve como un objeto `Result`.

También hay otros métodos disponibles en el paquete `reqwest`, como `post()`, `put()` y `delete()` que se pueden utilizar para enviar solicitudes HTTP con otros métodos.

## Ver también

- [Documentación oficial de reqwest](https://docs.rs/reqwest/)
- [Tutorial de HTTP en Rust](https://stevedonovan.github.io/hyperrust/)
- [Ejemplos de reqwest](https://github.com/seanmonstar/reqwest/tree/master/examples)