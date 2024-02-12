---
title:                "Enviando una solicitud http con autenticación básica"
aliases: - /es/rust/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:30.152586-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

Enviar una solicitud HTTP con autenticación básica es el proceso de comunicarse con un servidor web usando credenciales codificadas en Base64 (usuario:contraseña). Los programadores lo hacen para acceder a recursos protegidos o para interactuar con APIs que requieren autenticación.

## Cómo hacerlo:

En Rust, puedes usar la biblioteca `reqwest` para manejar solicitudes HTTP. Vamos a ver cómo implementarlo.

Primero, añade `reqwest` a tu archivo `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
base64 = "0.13"
```

Luego, en tu archivo principal:

```rust
use reqwest::header::{Authorization, Basic};
use base64::encode;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let username = "mi_nombre_de_usuario";
    let password = "mi_contraseña";
    let encoded_credentials = encode(&format!("{}:{}", username, password));
    
    let client = reqwest::Client::new();
    let response = client.get("https://mi.servidor.com/recurso")
        .header(Authorization(Basic {
            username: username.to_string(),
            password: Some(password.to_string()),
        }))
        .send()
        .await?;
    
    println!("Status: {}", response.status());
    println!("Headers:\n{:#?}", response.headers());
    
    let body = response.text().await?;
    println!("Body:\n{}", body);
    
    Ok(())
}
```

Al ejecutar este código, deberías ver la salida del status de la solicitud y detalles de la respuesta. Asegúrate de que `tokio` también esté añadido a tus dependencias si piensas usar `async`.

## Análisis Profundo

La autenticación básica es un método antiguo pero aún utilizado. Es parte del protocolo HTTP desde la versión 1.0. Aunque es simple, no es la opción más segura porque la credencial se envía en texto claro codificada en Base64, fácil de decodificar.

Existen alternativas más seguras como OAuth y JWT (JSON Web Tokens). Sin embargo, la autenticación básica se mantiene popular para ciertos contextos donde la facilidad y rapidez de implementación son claves, como al desarrollar y probar APIs internas.

Técnicamente, la autenticación básica se implementa incluyendo el encabezado `Authorization` con el valor `Basic`, seguido de las credenciales codificadas. Rust's `reqwest` y `base64` simplifican este proceso manejando el encoding y el manejo de los headers automáticamente.

## Ver También

Para más información, explorar los siguientes enlaces puede ser útil:

- [reqwest crate documentation](https://docs.rs/reqwest/)
- [The Rust `async` Book](https://rust-lang.github.io/async-book/)
- [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [MDN Web Docs on HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [base64 crate documentation](https://docs.rs/base64/)

Este contenido te ayudará a profundizar tu conocimiento sobre la autenticación HTTP básica en Rust y otros métodos de autorización.
