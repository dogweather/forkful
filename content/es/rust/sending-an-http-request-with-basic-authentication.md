---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Enviar una solicitud HTTP con autenticación básica es acreditar una solicitud HTTP con usuario y password. Los programadores lo hacen cuando quieren interactuar con APIs protegidas que requieren autenticación.

## Cómo hacerlo:

```Rust
use reqwest::blocking::Client;
use reqwest::header::{HeaderMap, HeaderValue, AUTHORIZATION};

fn main() {
    let client = Client::new();
    let mut headers = HeaderMap::new();
    headers.insert(AUTHORIZATION, HeaderValue::from_static("Basic usuario:password"));
    let res = client.get("https://ejemplo.com").headers(headers).send();

    match res {
        Ok(response) => println!("Respuesta exitosa: {:?}", response),
        Err(error) => println!("Ocurrió un error: {:?}", error),
    }
}
```

## Análisis en profundidad

La autenticación HTTP básica es uno de los métodos más antiguos para autenticar usuarios en la web. Sin embargo, debido a su simplicidad, también es vulnerable a varios ataques si no se utiliza con una conexión segura (HTTPS).

Una alternativa al autenticación básica es la autenticación de portador del token. Este método implica enviar un token en el encabezado de la autorización en lugar de las credenciales en texto plano.

En cuanto a la implementación con Rust, el paquete "reqwest" es una opción popular debido a su simplicidad y facilidad de uso. Además, permite personalizar los encabezados de las solicitudes HTTP, lo que facilita la autenticación básica.

## Ver también

1. Documentación oficial de Rust: https://doc.rust-lang.org/rust-by-example/
2. 'reqwest' en Crates.io: https://crates.io/crates/reqwest
3. Autenticación HTTP básica en Wikipedia: https://es.wikipedia.org/wiki/Autenticación_de_acceso_básica