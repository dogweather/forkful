---
title:                "Enviando una solicitud http"
html_title:           "Gleam: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# ¿Por qué enviar una solicitud HTTP en Gleam?

Hay muchas razones por las que podría querer enviar una solicitud HTTP en Gleam. Algunos ejemplos podrían ser para obtener datos de una API, enviar información del usuario a un servidor o realizar acciones en una aplicación web.

## Cómo hacerlo

Aquí hay un ejemplo de cómo enviar una solicitud GET en Gleam y obtener los datos de una API:

```Gleam
import http
import gleam/json

let res =
  http.get("https://ejemplo/api/?id=1")
  |> http.send()

let body = res.body
```

La variable `res` contiene toda la respuesta de la solicitud, incluyendo el cuerpo de la respuesta, las cabeceras y el código de estado. Luego podemos usar el módulo `gleam/json` para analizar el cuerpo de la respuesta y obtener los datos en un formato útil.

También es posible enviar solicitudes POST y enviar datos junto con la solicitud. Aquí hay un ejemplo de cómo enviar una solicitud POST con datos en formato JSON:

```Gleam
import http
import gleam/json

let data = json.encode([{"name": "Juan", "age": 25}])
let res =
  http.post("https://ejemplo/api/users")
  |> http.add_header("Content-Type", "application/json")
  |> http.set_body(data)
  |> http.send()

let body = res.body
```
El código anterior crea una solicitud POST con un encabezado de tipo de contenido de `application/json` y el cuerpo de la solicitud es el objeto `data` convertido en formato JSON. Luego se envía la solicitud y se obtiene la respuesta.

## Inmersión profunda

Sending HTTP requests in Gleam is made possible by using the `http` module. This module provides functions for creating and managing HTTP requests, including methods for adding headers, setting the body, and sending the request. It also has functions for managing the response, such as accessing the body and headers.

Behind the scenes, the `http` module uses the underlying `httpc` Erlang library to handle the actual communication with the server. This means that Gleam has a powerful and reliable foundation for handling HTTP requests.

# Ver también

- Documentación oficial para el módulo `http` en Gleam: https://gleam.run/modules/http.html
- Tutorial sobre cómo trabajar con datos JSON en Gleam: https://gleam.run/tutorials/json.html
- Ejemplos de código en Gleam: https://github.com/gleam-lang/gleam/tree/master/examples