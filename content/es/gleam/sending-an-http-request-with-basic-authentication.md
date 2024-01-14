---
title:                "Gleam: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Si estás trabajando en una aplicación que se comunica con un servidor a través de solicitudes HTTP, es posible que necesites enviar solicitudes autenticadas para acceder a datos o realizar acciones protegidas. En este caso, es importante comprender cómo enviar una solicitud HTTP con autenticación básica.

## Cómo hacerlo

Puedes enviar una solicitud HTTP con autenticación básica utilizando la biblioteca HTTP incorporada en Gleam. Primero, necesitas importar los módulos necesarios:

```Gleam
import http
import base64
```

Luego, puedes crear las credenciales para la autenticación básica utilizando el nombre de usuario y la contraseña requeridos por el servidor:

```Gleam
let username = "usuario"
let password = "contraseña"
let credentials = base64.encode(username ++ ":" ++ password)
```

Después de tener las credenciales, puedes construir la solicitud HTTP con la función `Request.basic_auth` y especificar la ruta y los encabezados necesarios:

```Gleam
let request = http.request(
  method = http.Method.Get,
  url = "https://servidor.com/datos",
  mut_headers = [ http.header("Authorization", "Basic " ++ credentials) ],
  body = http.empty(),
)
```

Finalmente, puedes enviar la solicitud utilizando la función `send` y manejar la respuesta de la misma manera que cualquier otra solicitud HTTP:

```Gleam
let response = http.send(request)
match response.body {
  Err(err)       -> "Error al enviar la solicitud: #{err}"
  Ok(response)   -> response.body
}
```

## Buceo profundo

La autenticación básica es uno de los métodos más simples de autenticación en HTTP. Funciona enviando las credenciales del usuario en texto plano, por lo que es importante usarlo solo en conexiones seguras (HTTPS). Además, a pesar de ser simple, es importante tener en cuenta que no es el método de autenticación más seguro y se recomienda investigar y utilizar métodos más avanzados si la seguridad es una preocupación en tu aplicación.

## Ver también

- [Documentación de la biblioteca HTTP en Gleam](https://gleam.run/modules/http.html)
- [Documentación de la biblioteca base64 en Gleam](https://gleam.run/modules/base64.html)
- [Especificación de autenticación básica en HTTP](https://tools.ietf.org/html/rfc7617)