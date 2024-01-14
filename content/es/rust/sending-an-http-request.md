---
title:                "Rust: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por qué enviar una solicitud HTTP en Rust

¿Alguna vez te has preguntado cómo se realizan las solicitudes y respuestas en la web? Bueno, en Rust, podemos utilizar el poderoso módulo de red para enviar y recibir datos a través de HTTP. Ya sea para interactuar con una API, obtener información de un sitio web o realizar pruebas, enviar una solicitud HTTP en Rust puede ser una habilidad útil para cualquier programador.

## Cómo hacerlo

Primero, debemos importar el módulo de red de Rust:

```Rust
use std::net::TcpStream;
```

Luego, definimos la URL a la que queremos hacer la solicitud y creamos un socket TCP para conectarnos:

```Rust
let url = "https://example.com";
let stream = TcpStream::connect(url).expect("Error de conexión");
```

A continuación, podemos enviar una solicitud GET y recibir la respuesta del servidor en una variable mutable:

```Rust
use std::io::prelude::*;

let mut response = String::new();

stream.write_all(b"GET / HTTP/1.1\r\nHost: example.com\r\n\r\n").expect("Error al escribir en el socket");

stream.read_to_string(&mut response).expect("Error al leer la respuesta del servidor");
```

Finalmente, podemos imprimir la respuesta en la consola:

```Rust
println!("{}", response);
```

Si todo sale bien, deberíamos ver la página de inicio de "example.com" como resultado.

## Profundizando en el envío de solicitudes HTTP

Al enviar una solicitud HTTP en Rust, es importante recordar que estamos enviando bytes en lugar de cadenas de texto. Por lo tanto, debemos asegurarnos de convertir nuestras cadenas a bytes utilizando el método `b""`.

También es importante utilizar los encabezados adecuados en nuestras solicitudes para que el servidor pueda procesarlas correctamente. En el ejemplo anterior, agregamos el encabezado "Host" para especificar a qué sitio web estamos enviando la solicitud.

Además, es importante manejar posibles errores en la conexión y la lectura / escritura de datos en el socket. Para ello, podemos utilizar los métodos `expect()` y `unwrap()`.

# Consulte también

- [Documentación oficial de la biblioteca std::net de Rust](https://doc.rust-lang.org/std/net/index.html)
- [Tutorial de Rust: Envío de solicitudes HTTP](https://www.youtube.com/watch?v=k9xPtd73bbQ)
- [Ejemplos de código de envío de solicitudes HTTP en Rust](https://github.com/tower-rs/tower-http)