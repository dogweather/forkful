---
title:                "Enviando una solicitud http"
html_title:           "Go: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has navegado por una página web y te has preguntado cómo funciona el intercambio de información entre tu navegador y el servidor? Bueno, eso es gracias a las solicitudes HTTP. En este artículo, vamos a explorar cómo puedes enviar una solicitud HTTP utilizando Go.

## Cómo hacerlo

Todo lo que necesitas para enviar una solicitud HTTP en Go son dos componentes: un cliente HTTP y una URL de destino. Primero, importa el paquete `"net/http"` en tu código. Luego, puedes crear un cliente HTTP utilizando la función `http.Client()`:

```Go
import "net/http"

cliente := http.Client()
```

Luego, debes crear una solicitud HTTP con la URL de destino y el método HTTP adecuado. Por ejemplo, si quieres obtener el contenido de una página web, puedes utilizar el método `GET`:

```Go
solcitud, error := http.NewRequest("GET", "https://www.ejemplo.com", nil)
if error != nil {
    // manejo de errores
}
```

Ahora solo queda ejecutar la solicitud utilizando el cliente que creamos anteriormente y manejar la respuesta. Puedes hacerlo con la función `cliente.Do()` y el método de respuesta `Response.Body`. Aquí hay un ejemplo de cómo imprimir el contenido de la respuesta:

```Go
respuesta, error := cliente.Do(solicitud)
if error != nil {
    // manejo de errores
}
defer respuesta.Body.Close()

fmt.Println(respuesta.Body)
```

¡Y eso es todo! Con solo unos pocos pasos sencillos, has enviado una solicitud HTTP utilizando Go.

## Profundizando

Ahora que ya sabes cómo enviar una solicitud HTTP en Go, es importante comprender los diferentes componentes y opciones que están disponibles. Por ejemplo, puedes agregar encabezados personalizados a tu solicitud utilizando el método `Request.Header.Add()` y también puedes establecer un tiempo de espera para la solicitud utilizando la función `Timeout` del cliente HTTP.

También es importante tener en cuenta cómo manejar los errores adecuadamente al enviar solicitudes HTTP. Puedes utilizar declaraciones `if` o el paquete `"log"` para registrar los errores y asegurarte de que tu código funcione de manera confiable.

## Ver también

- [Documentación de Go sobre el paquete "net/http"](https://golang.org/pkg/net/http/)
- [Ejemplo de solicitud HTTP en Go](https://gobyexample.com/http-client)
- [Guía de estilo de Go](https://github.com/golang/go/wiki/CodeReviewComments#formatting)