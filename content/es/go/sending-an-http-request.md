---
title:                "Enviando una solicitud http"
date:                  2024-01-20T17:59:51.360954-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Enviar una solicitud HTTP es cómo tu programa en Go pide información o manda datos a otro sistema en la web. Hacemos esto para interactuar con APIs, servicios web o cualquier recurso en internet desde nuestro código.

## Cómo hacerlo:
```Go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    // Realizar una solicitud GET
    respuesta, err := http.Get("https://jsonplaceholder.typicode.com/posts/1")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer respuesta.Body.Close()

    // Leer y mostrar el cuerpo de la respuesta
    cuerpo, err := ioutil.ReadAll(respuesta.Body)
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Println(string(cuerpo))
}
```
Ejecutando esto deberías ver algo así:
```
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit..."
}
```

## Profundizando
En el pasado, las solicitudes se hacían con herramientas como `curl` en la línea de comandos o bibliotecas diseñadas específicamente para lenguajes de programación. Go incluye un potente paquete `net/http` para manejar solicitudes HTTP que facilita todo este proceso.

Hay diferentes métodos HTTP: GET, POST, PUT, DELETE, etc. `http.Get` es cómodo para una solicitud GET simple, pero para algo más elaborado, puedes usar `http.NewRequest` y personalizar a tu gusto.

Cuando envías una solicitud, es importante manejar errores y cerrar el cuerpo de respuesta con `defer`. Esto evita fugas de recursos y problemas de rendimiento en programas más largos.

Alternativas incluyen goroutines y canales para manejar respuestas asíncronas, o usar librerías de terceros que ofrecen más funcionalidades o simplifican algunas tareas.

## Ver También
- [Documentación oficial del paquete `net/http`](https://pkg.go.dev/net/http)
- [Tutorial de ‘Making HTTP Requests’ en Go by Go By Example](https://gobyexample.com/http-clients)
- [Artículo sobre el manejo de errores en Go por Dave Cheney](https://dave.cheney.net/2012/01/18/why-go-gets-exceptions-right)
