---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Gleam: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# ¡Enviar solicitudes HTTP con autenticación básica en Gleam!

## ¿Qué y por qué?

En programación, a menudo necesitamos enviar solicitudes HTTP para comunicarnos con otros servicios en línea. Sin embargo, en algunos casos, el acceso a estos servicios requiere autenticación básica. Esto significa que debemos proporcionar un nombre de usuario y una contraseña para acceder a los recursos del servicio. En Gleam, podemos hacer esto fácilmente con unos pocos pasos.

## Cómo:

Podemos enviar solicitudes HTTP con autenticación básica en Gleam utilizando la biblioteca [Httpc](https://github.com/gleam-lang/httpc). Primero, debemos agregar esta biblioteca a nuestro proyecto en nuestro archivo `gleam.toml`. Luego, podemos usar el módulo `Httpc.Request` para enviar nuestra solicitud.

```Gleam
import httpc

// Definir nuestro nombre de usuario y contraseña
let username = "usuario"
let password = "contraseña"

// Crear una solicitud HTTP utilizando el método `basic_auth`
let request = httpc.Request.basic_auth("https://servicio.com/endpoint", username, password)

// Realizar la solicitud
httpc.send(request)
```

Esto enviará una solicitud HTTP a la URL especificada con el encabezado de autenticación básica incluido. Si la autenticación es exitosa, recibiremos una respuesta del servicio.

## Profundizando:

En el pasado, se utilizaba ampliamente la autenticación básica en servicios en línea, pero con el tiempo ha sido reemplazada por métodos de autenticación más seguros como OAuth. Sin embargo, aún podemos encontrarnos con servicios que utilizan autenticación básica, por lo que es útil saber cómo implementarla en Gleam.

Otra forma de enviar solicitudes HTTP con autenticación básica es a través de la biblioteca [Erlang](https://github.com/gleam-lang/gleam-lib-erlang/httpc). Esta biblioteca de bajo nivel nos permite interactuar directamente con el sistema de HTTP de Erlang, lo que puede ser útil en situaciones más complejas.

## Ver también:

- Documentación de [Httpc](https://github.com/gleam-lang/httpc)
- Biblioteca [Erlang](https://github.com/gleam-lang/gleam-lib-erlang/httpc) para enviar solicitudes HTTP con autenticación básica en Gleam