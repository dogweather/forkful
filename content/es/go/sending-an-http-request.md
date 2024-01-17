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

## ¿Qué y por qué?

Enviar una solicitud HTTP es un proceso en el que un programador utiliza un lenguaje de programación para solicitar información de un servidor a través de la web. Los programadores lo hacen para obtener datos o interactuar con una aplicación o sitio web.

## ¡A por ello!

```Go
import "net/http"

// Crear una solicitud GET a una dirección URL
resp, err := http.Get("https://ejemplo.com")

// Comprobar si hubo un error
if err != nil {
    fmt.Println("Error al realizar la solicitud HTTP:", err)
}

// Imprimir el código de estado de la respuesta
fmt.Println("Código de estado:", resp.Status)

// Leer y mostrar el cuerpo de la respuesta
body, err := ioutil.ReadAll(resp.Body)
fmt.Println("Cuerpo de la respuesta:", string(body))
```

Output: 
Código de estado: 200 OK
Cuerpo de la respuesta: ¡Hola mundo!

## Profundizando

El envío de solicitudes HTTP ha existido desde los primeros días de la web, ya que es una forma básica y crucial de comunicación entre servidores y clientes. Aunque la mayoría de los lenguajes de programación tienen la capacidad de enviar solicitudes HTTP, Go ofrece una sintaxis limpia y sencilla, y también cuenta con bibliotecas específicas como "net/http" que facilitan el proceso.

Alternativas a Go para el envío de solicitudes HTTP incluyen lenguajes como Python o JavaScript, que también tienen bibliotecas y frameworks dedicados a esto. Sin embargo, Go es una excelente opción para aplicaciones de alto rendimiento debido a su capacidad para manejar múltiples solicitudes simultáneamente de forma eficiente.

## ¡Te recomiendo!

- [Documentación oficial de Go sobre solicitudes HTTP](https://golang.org/pkg/net/http/)
- [Tutorial sobre cómo enviar solicitudes HTTP en Go](https://golangbyexample.com/http-get-request-go/)
- [Artículo sobre comparación de velocidad entre diferentes lenguajes para el envío de solicitudes HTTP](https://dzone.com/articles/efficiency-of-http-request-parsing-in-different-l)