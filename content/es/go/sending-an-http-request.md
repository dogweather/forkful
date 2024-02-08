---
title:                "Enviando una solicitud HTTP"
aliases:
- es/go/sending-an-http-request.md
date:                  2024-02-03T18:08:29.597722-07:00
model:                 gpt-4-0125-preview
simple_title:         "Enviando una solicitud HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP involucra iniciar una llamada desde tu aplicación Go a un servidor web, API o cualquier otro servicio basado en HTTP. Los programadores hacen esto para interactuar con recursos web, obtener datos, enviar formularios o comunicarse con otros servicios a través de Internet.

## Cómo hacerlo:

En Go, enviar una solicitud HTTP y manejar la respuesta implica usar el paquete `net/http`. Aquí tienes un ejemplo paso a paso sobre cómo enviar una simple solicitud GET y leer la respuesta:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "net/http"
)

func main() {
    // Define la URL del recurso
    url := "http://example.com"

    // Usa http.Get para enviar la solicitud GET
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }
    // Cierra el cuerpo de respuesta cuando la función termina
    defer resp.Body.Close()

    // Lee el cuerpo de respuesta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    // Convierte el cuerpo de respuesta a un string e imprímelo
    fmt.Println(string(body))
}
```

Salida de muestra (acortada por brevedad):
```
<!doctype html>
<html>
<head>
    <title>Dominio de ejemplo</title>
...
</html>
```

Para enviar una solicitud POST con datos de formulario, puedes usar `http.PostForm`:

```go
package main

import (
    "fmt"
    "io/ioutil"
    "net/http"
    "net/url"
)

func main() {
    // Define la URL y los datos del formulario
    url := "http://example.com/form"
    data := url.Values{}
    data.Set("clave", "valor")

    // Envía la solicitud POST con datos de formulario
    resp, err := http.PostForm(url, data)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()

    // Lee e imprime la respuesta
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }

    fmt.Println(string(body))
}
```

## Análisis Profundo

El paquete `net/http` en Go ofrece una forma poderosa y flexible de interactuar con servidores HTTP. Su diseño refleja el énfasis de Go en la simplicidad, eficiencia y robustez. Originalmente, funcionalidades como manejar cargas útiles JSON o XML requerían la creación manual del cuerpo de la solicitud y la configuración de encabezados apropiados. A medida que Go evolucionó, la comunidad ha desarrollado paquetes de nivel superior que simplifican aún más estas tareas, como `gorilla/mux` para enrutamiento y `gjson` para manipulación de JSON.

Un aspecto notable del cliente HTTP de Go es su uso de interfaces y estructuras, como `http.Client` y `http.Request`, que permiten una amplia personalización y prueba. Por ejemplo, puedes modificar el `http.Client` para que las solicitudes tengan un tiempo de espera o mantener las conexiones activas para mejorar el rendimiento.

Una alternativa considerada para interacciones HTTP más simples es el uso de bibliotecas de terceros como "Resty" o "Gentleman". Estos paquetes ofrecen una abstracción de nivel más alto para las solicitudes HTTP, haciendo que las tareas comunes sean más concisas. Sin embargo, entender y utilizar el paquete subyacente `net/http` es crucial para lidiar con escenarios de interacción HTTP más complejos o únicos, proporcionando una base sobre la cual se pueden aprovechar completamente las características de concurrencia de Go y su poderosa biblioteca estándar.
