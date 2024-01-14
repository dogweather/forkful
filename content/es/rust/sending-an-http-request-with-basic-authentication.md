---
title:                "Rust: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Por qué enviar una solicitud HTTP con autenticación básica en Rust?

Si eres un desarrollador de Rust, es probable que hayas oído hablar de la importancia de la seguridad en las aplicaciones web. Una forma de asegurar tu aplicación es a través de la autenticación básica en las solicitudes HTTP. En este artículo, aprenderemos cómo enviar una solicitud utilizando autenticación básica en Rust, asegurando así que nuestra aplicación sea más segura.

## Cómo hacerlo en Rust

Para enviar una solicitud HTTP con autenticación básica en Rust, primero necesitamos usar la biblioteca `reqwest`. Esta biblioteca nos permitirá realizar solicitudes HTTP de manera fácil y segura. Primero, debemos agregar `reqwest` a nuestro archivo `Cargo.toml`:

```rust 
[dependencias]
reqwest = "0.11.0"
```

Luego, en nuestro archivo `main.rs`, importamos la biblioteca y las otras dependencias necesarias:

```rust 
use std::io::Read;
use reqwest::Client;
```

Ahora podemos crear una instancia de `Client` para manejar nuestras solicitudes HTTP:

```rust
let client = Client::new();
```

Para enviar una solicitud con autenticación básica, necesitamos proporcionar un nombre de usuario y una contraseña en un encabezado llamado `Authorization`. Podemos hacer esto fácilmente utilizando el método `basic_auth` de `Client`:

```rust
let mut response = client
    .get("https://ejemplo.com")
    .basic_auth("usuario", Some("contraseña"))
    .send()
    .expect("Error al enviar la solicitud");
```

En este ejemplo, estamos enviando una solicitud GET a `https://ejemplo.com` con un nombre de usuario y una contraseña en el encabezado de autorización. Si la solicitud es exitosa, podemos leer la respuesta utilizando el método `read_to_string`:

```rust
let mut body = String::new();
response.read_to_string(&mut body).expect("Error al leer la respuesta");
println!("Respuesta: {}", body);
```

Con esto, hemos enviado con éxito una solicitud HTTP utilizando autenticación básica en Rust.

## Profundizando en la autenticación básica en Rust

Si te interesa aprender más sobre la autenticación básica en Rust, puedes consultar los siguientes recursos:

- Documentación de `reqwest`: https://docs.rs/reqwest/0.11.0/reqwest/
- Ejemplo de autenticación básica en Rust: https://github.com/seanmonstar/reqwest/blob/master/examples/basic_auth.rs
- Tutorial de Rust sobre autenticación básica: https://www.lihautan.com/authentication-in-rust/
- Artículo sobre la importancia de la seguridad en aplicaciones web: https://devblogs.microsoft.com/premier-developer/securing-your-web-applications-using-rust/

# Ver También

- Cómo manejar errores en las solicitudes HTTP en Rust: https://github.com/jonathan-s/rust-http-error-handling
- Tutorial de Rust sobre cómo hacer solicitudes HTTP: https://dev.to/jeikabu/http-requests-in-rust-4gmo