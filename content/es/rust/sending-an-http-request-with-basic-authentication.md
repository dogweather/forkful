---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Rust: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Hay muchas razones por las cuales uno podría querer enviar una solicitud HTTP con autenticación básica en Rust. Por ejemplo, al comunicarse con una API que requiere autenticación para acceder a ciertos recursos o datos protegidos, o al crear una aplicación que requiere una forma de identificación segura para utilizar.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Rust, primero debemos importar las siguientes dependencias:

```Rust
use reqwest::blocking::{Client, Response};
use reqwest::header::{HeaderValue, BASIC_AUTH};
```

A continuación, creamos un cliente de solicitud e inicializamos las credenciales de autenticación con un nombre de usuario y una contraseña:

```Rust
let client = Client::new();
let username = "usuario";
let password = "contraseña";
```

Luego, construimos una solicitud HTTP utilizando el método `get` y agregamos las credenciales de autenticación al encabezado para protegerla:

```Rust
let request = client.get("https://www.ejemplo.com/api/datos")
    .header(BASIC_AUTH, HeaderValue::from_str(&format!("{}:{}", username, password)).unwrap());
```

Por último, enviamos la solicitud y obtenemos la respuesta del servidor:

```Rust
let response: Response = match request.send() {
    Ok(response) => response,
    Err(e) => panic!("Error al enviar la solicitud: {}", e),
};
```

Una vez que tenemos la respuesta, podemos manipularla según sea necesario. Por ejemplo, podemos obtener el código de estado de la respuesta:

```Rust
println!("Código de estado: {}", response.status());
```

## Inmersión profunda

Ahora que sabemos cómo enviar una solicitud HTTP con autenticación básica en Rust, podemos profundizar un poco más en cómo funciona esto. La autenticación básica utiliza el encabezado `Authorization` en la solicitud HTTP para enviar las credenciales de autenticación al servidor.

En Rust, esto se logra utilizando la constante `BASIC_AUTH` del módulo `header` de la biblioteca `reqwest`. Esta constante representa el encabezado `Authorization` y se utiliza en conjunción con la función `HeaderValue::from_str()` para construir un encabezado con las credenciales de usuario y contraseña codificadas en base64.

Una vez que se envía la solicitud con autenticación básica, el servidor recibirá las credenciales y las comparará con las almacenadas en su base de datos. Si coinciden, se otorgará acceso a los recursos protegidos.

## Ver también

- [Documentación oficial de reqwest](https://docs.rs/reqwest)
- [Tutorial de Rust: Introducción a la programación web](https://www.rust-lang.org/es-ES/learn/get-started)