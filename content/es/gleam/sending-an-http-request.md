---
title:                "Enviando una solicitud http"
date:                  2024-01-20T17:59:48.484459-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Enviar una petición HTTP es simplemente el acto de solicitar datos a otro servidor a través de la web. Lo hacemos para interactuar con recursos remotos, ya sea para recuperar datos, enviar información o integrar diferentes servicios y aplicaciones.

## Cómo hacerlo:

Vamos al grano. Para hacer una petición HTTP en Gleam, necesitarás un cliente HTTP como `gleam/http`. Imagina que queremos obtener algún dato JSON de un servicio web.

```gleam
import gleam/http
import gleam/should

pub fn main() {
  // Define la URL a la que quieres enviar la petición
  let url = "https://jsonplaceholder.typicode.com/todos/1"
  let response = http.get(url)

  case response {
    Ok(response) -> 
      // Si todo va bien, aquí manejas la respuesta
      should.equal(response.status, 200)
      should.equal(response.body, #"""{"userId": 1, "id": 1, ...}""")
    Error(error) ->
      // Si algo sale mal, manejar el error aquí
      io.println("Algo salió mal: " ++ error)
  }
}
```
Ejecutas tu código, y si todo sale bien, verás algo como:

```
"userId": 1, "id": 1, ...
```

## Conociendo más a fondo:

La capacidad de enviar peticiones HTTP no es algo nuevo, se remonta a los inicios de la web, permitiendo la comunicación entre clientes y servidores. Antes de Gleam, podrías haber usado Curl o bibliotecas de diferentes lenguajes como `Net::HTTP` de Ruby o `requests` de Python. En Gleam, la simplicidad y seguridad del tipado estático se combina para hacer que tus peticiones sean más manejables y menos propensas a errores. Bajo el capó, Gleam hace uso de Erlang's OTP, lo que brinda robustez y confiabilidad en la comunicación entre servicios a través de HTTP.

## Véase También:

Para seguir ampliando tus conocimientos en Gleam y peticiones HTTP, aquí tienes algunos recursos:

- Un tutorial para principiantes en Gleam: [https://gleam.run/book/tour](https://gleam.run/book/tour)
- JSONPlaceholder, un servicio para probar peticiones HTTP: [https://jsonplaceholder.typicode.com/](https://jsonplaceholder.typicode.com/)

Recuerda, la práctica es clave. Sigue codificando y experimentando con diferentes endpoints y tipos de peticiones. ¡Feliz codificación!
