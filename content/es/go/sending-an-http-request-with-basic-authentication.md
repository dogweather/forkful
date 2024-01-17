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

¿Qué es enviar una solicitud HTTP con autenticación básica? ¿Por qué los programadores lo hacen?

Enviar una solicitud HTTP con autenticación básica significa agregar credenciales de usuario en el encabezado de la solicitud para acceder a una página web o recurso protegido. Los programadores lo hacen para asegurar que solo los usuarios autorizados puedan acceder a la información protegida, protegiendo así la privacidad y seguridad de los usuarios.

## Cómo hacerlo:

En Go, puedes enviar una solicitud HTTP con autenticación básica utilizando la función ```http.NewRequest()``` y agregando las credenciales en el encabezado de la solicitud. Aquí hay un ejemplo de código que envía una solicitud GET con autenticación básica:

```
req, err := http.NewRequest("GET", "website.com/resource", nil)
if err != nil {
  log.Fatal(err)
}

// Agregar credenciales en el encabezado de la solicitud
req.SetBasicAuth("username", "password")

// Realizar la solicitud
resp, err := http.DefaultClient.Do(req)
if err != nil {
  log.Fatal(err)
}
defer resp.Body.Close()

// Imprimir el resultado de la solicitud
fmt.Println("Codigo de estado: ", resp.StatusCode)
fmt.Println("Respuesta: ", resp.Status)
```

El resultado de la solicitud será el código de estado y la respuesta correspondiente, por ejemplo: "200 OK".

## Profundizando:

La autenticación básica HTTP ha existido desde 1999 y fue diseñada para un uso simple y no sofisticado. Sin embargo, en la actualidad, se considera una forma insegura de autenticación, ya que las credenciales se envían en texto plano y pueden ser interceptadas por terceros. Existen alternativas más seguras, como HTTPS y OAuth, que utilizan técnicas de encriptación para proteger las credenciales del usuario.

Sin embargo, la autenticación básica HTTP sigue siendo ampliamente utilizada debido a su simplicidad y facilidad de implementación. Siempre es importante considerar la seguridad de la información y evaluar si la autenticación básica HTTP es la opción más adecuada para tu proyecto.

## Ver también:

- Documentación oficial de Go para el paquete "net/http": https://golang.org/pkg/net/http/
- Tutorial sobre cómo realizar solicitudes HTTP en Go: https://tour.golang.org/concurrency/10
- Más información sobre autenticación básica HTTP: https://tools.ietf.org/html/rfc7617