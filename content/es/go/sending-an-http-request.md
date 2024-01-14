---
title:                "Go: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

El envío de solicitudes HTTP es una parte fundamental de la programación en Go. Al utilizar este método, podemos comunicarnos con servidores web y obtener información de manera eficiente. Aprender a enviar solicitudes HTTP es una habilidad esencial para cualquier programador de Go.

## Cómo

Para enviar una solicitud HTTP en Go, necesitamos utilizar la biblioteca estándar "net/http". Primero, importamos esta biblioteca en nuestro archivo de código:

```Go
import "net/http"
```

Luego, creamos una función que maneje la solicitud HTTP. En este ejemplo, enviaremos una solicitud GET al sitio web de Google y obtendremos su código de estado de respuesta:

```Go
func main() {
    response, err := http.Get("https://www.google.com/")
    if err != nil {
        panic(err)
    }

    fmt.Println(response.Status)
}
```

En este código, utilizamos la función "http.Get" para enviar una solicitud GET a la URL proporcionada. Guardamos la respuesta en la variable "response" y manejamos cualquier error que pueda surgir. Luego, imprimimos el código de estado de la respuesta utilizando la propiedad "Status" de la variable "response".

## Deep Dive

El paquete "net/http" también nos permite personalizar nuestra solicitud HTTP con diferentes opciones. Por ejemplo, podemos agregar encabezados a nuestra solicitud para proporcionar información adicional al servidor.

```Go
req, err := http.NewRequest("GET", "https://www.example.com/", nil)
req.Header.Set("User-Agent", "My-Go-Client") //agregamos un encabezado personalizado
response, err := http.DefaultClient.Do(req)
```

En este código, utilizamos la función "NewRequest" para crear una nueva solicitud GET. Luego, agregamos un encabezado con la función "Set" y utilizamos la función "Do" para enviar nuestra solicitud personalizada utilizando el cliente HTTP predeterminado.

## Ver también

- Documentación oficial de "net/http": https://golang.org/pkg/net/http/
- Tutorial de envío de solicitudes HTTP en Go: https://gowebexamples.com/http-client/
- Ejemplos de código de solicitud HTTP en Go: https://gobyexample.com/http-clients