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

## ¿Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es un método que utilizan los programadores para proteger sus aplicaciones web de accesos no autorizados. Básicamente, es una forma de solicitar credenciales de usuario para permitir el acceso a ciertas partes de una aplicación.

## Cómo hacerlo:

```Rust
use reqwest::blocking::Client;
 
// Crear un cliente HTTP con autenticación básica
let client = Client::new()
    .basic_auth("username", Some("password"))
    .build()?;
 
// Enviar una solicitud GET a una URL con autenticación básica
let res = client.get("https://example.com")
    .send()?;
 
println!("{:?}", res); // Imprimir la respuesta HTTP
```

La salida del código anterior variará dependiendo de la respuesta del servidor web.

## Profundizando

La autenticación básica en HTTP es un método de autenticación más antiguo y menos seguro en comparación con otros métodos más modernos como OAuth. Sin embargo, todavía se utiliza ampliamente en aplicaciones que no manejan información sensible.

Existen alternativas a la autenticación básica, como la autenticación de tokens o el uso de claves API. Estas opciones son más seguras y generalmente se recomiendan para aplicaciones que manejan información confidencial.

El envío de una solicitud HTTP con autenticación básica se realiza mediante el envío de un encabezado de autorización en la solicitud, que incluye un nombre de usuario y una contraseña en formato codificado.

## Ver también

- [Documentación de Rust para HTTP básico](https://doc.rust-lang.org/reqwest/reqwest/struct.ClientBuilder.html#method.basic_auth)
- [Explicación de autenticación básica en HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Alternativas a autenticación básica en aplicaciones web](https://auth0.com/blog/amp/what-is-basic-authentication/)