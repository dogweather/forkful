---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Go: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una forma de asegurar la comunicación entre un cliente y un servidor. Es especialmente útil cuando se manejan contenidos sensibles o protegidos, ya que garantiza que solo los usuarios autorizados puedan acceder a ellos.

## Cómo Hacerlo

Para enviar una solicitud HTTP con autenticación básica en Go, se necesita importar las siguientes librerías: 

```Go
import (
    "fmt"
    "net/http"
    "encoding/base64"
)
```

Luego, se debe crear un cliente HTTP y establecer la autenticación básica con el nombre de usuario y la contraseña adecuados:

```Go
client := &http.Client{}

req, err := http.NewRequest("GET", "https://www.example.com", nil)
if err != nil {
    fmt.Println(err)
}

// Establecer las credenciales de autenticación básica
req.SetBasicAuth("usuario", "contraseña")

// Realizar la solicitud
resp, err := client.Do(req)
if err != nil {
    fmt.Println(err)
}

// Leer la respuesta
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    fmt.Println(err)
}

// Imprimir el cuerpo de la respuesta
fmt.Println(string(body))
```

El resultado de esto debería ser la respuesta HTTP del servidor.

## Un análisis más profundo

En una solicitud HTTP con autenticación básica, el cliente incluye un encabezado de autorización en la solicitud. Este encabezado contiene las credenciales del usuario codificadas en base64. El servidor luego decodifica las credenciales y si son válidas, permite el acceso al recurso solicitado. 

Es importante tener en cuenta que la autenticación básica solo proporciona una capa básica de seguridad, ya que las credenciales se envían en texto plano. Se recomienda utilizar una forma más segura de autenticación, como HTTPS, para garantizar la seguridad de las comunicaciones.

## Ver también

- Documentación oficial de la librería HTTP en Go: https://golang.org/pkg/net/http/
- Tutorial sobre autenticación básica en Go: https://www.geeksforgeeks.org/go-programming-language-authentication/
- Guía para trabajar con encabezados en Go: https://medium.com/@masnun/making-http-requests-in-golang-dd123379efe7