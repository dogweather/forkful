---
title:                "Go: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

¿Por qué enviar una solicitud HTTP con autenticación básica? 

La autenticación básica es una forma sencilla de verificar la identidad de un usuario en una solicitud HTTP. Esto es importante para proteger los datos y limitar el acceso a ciertas partes de una aplicación o sitio web.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Go, primero debemos importar el paquete "net/http" y crear una estructura de tipo http.Client para manejar la comunicación con el servidor. Luego, en el encabezado de la solicitud, agregamos un campo "Authorization" que contiene la palabra "Basic" seguida de un token que contiene el nombre de usuario y la contraseña codificados en base64.

```Go 
package main 

import ( 
    "net/http" 
    "fmt" 
    "encoding/base64" 
) 

func main() { 
    // Crear una nueva solicitud GET 
    req, err := http.NewRequest("GET", "https://www.ejemplo.com", nil) 
    if err != nil { 
        panic(err) 
    } 

    // Codificar en base64 el nombre de usuario y la constraseña 
    credentials := base64.StdEncoding.EncodeToString([]byte("usuario:contraseña")) 
    
    // Agregar token de autenticación en el encabezado 
    req.Header.Add("Authorization", "Basic " + credentials) 
    
    // Realizar la solicitud 
    client := &http.Client{} 
    resp, err := client.Do(req) 
    if err != nil { 
        panic(err) 
    } 
    // Mostrar la respuesta 
    fmt.Println(resp) 
    // Cerrar la respuesta 
    defer resp.Body.Close() 
}
```

El resultado de esta solicitud será una respuesta del servidor que confirmará si la autenticación fue exitosa y permitirá el acceso al recurso solicitado.

## Profundizando

La autenticación básica utiliza la codificación en base64 como método de seguridad, pero es importante resaltar que esto no es una forma segura de autenticación, ya que el usuario y la contraseña se envían en texto claro en cada solicitud. Además, esta forma de autenticación no permite ciertas características de seguridad, como el uso de tokens de acceso o la verificación en dos pasos.

Para mejorar la seguridad de nuestra aplicación, se recomienda implementar otros métodos de autenticación, como OAuth o JWT, que proporcionan una forma más segura y efectiva de verificar la identidad de un usuario.

## Ver También

- [Autenticación básica HTTP en Go](https://golang.org/pkg/net/http/#Request)
- [Ejemplo de autenticación básica en Go](https://golangers.com/en/articles/http-basic-authentication-in-go)
- [Seguridad en aplicaciones web Go](https://medium.com/rungo/secure-your-go-web-application-with-these-quick-steps-eb71c0aeee17)