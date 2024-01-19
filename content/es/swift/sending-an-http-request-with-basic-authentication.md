---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Enviando una Solicitud HTTP con Autenticación Básica en Swift 

## ¿Qué y Por qué?

El envío de solicitudes HTTP con autenticación básica es un método que utilizamos para acceder a recursos protegidos en la web. Como programadores, lo hacemos para garantizar que solo los usuarios autorizados tengan acceso a ciertos datos o funcionalidades.

## Cómo hacerlo:

Para enviar una solicitud HTTP con autenticación básica en Swift, usaremos la clase `URLRequest` y `URLSession`.

```Swift
import Foundation

let usuario = "tu_usuario"
let contraseña = "tu_contraseña"
let url = URL(string: "https://www.tuweb.com")!

var request = URLRequest(url: url)
let loginString = String(format: "%@:%@", usuario, contraseña)
let loginData = loginString.data(using: String.Encoding.utf8)!
let base64LoginString = loginData.base64EncodedString()

request.httpMethod = "GET"
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let error = error {
        print("Error: \(error)")
    } else if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Received data:\n\(str!)")
    }
}

task.resume()
```

Este código arroja como resultado la respuesta proveniente del servidor, o un error en caso de haberlo.

## Inmersión Profunda

Enviando solicitudes HTTP con autenticación básica es un concepto que ha existido por algún tiempo. Es parte del protocolo HTTP y ha sido la columna vertebral de la web por décadas.

Todavía se usa mucho hoy en día, pero también hay alternativas disponibles. JWT (JSON Web Tokens) y OAuth son dos ejemplos de sistemas de autenticación más modernos que proporcionan un nivel de seguridad adicional.

En cuanto a la implementación en Swift, `URLRequest` y `URLSession` son las herramientas básicas que nos proporciona la biblioteca `Foundation`.

## Ver También

- Documentación de Apple: URLRequest ([enlace](https://developer.apple.com/documentation/foundation/urlrequest))
- Guía de autenticación HTTP básica: ([enlace](https://tools.ietf.org/html/rfc7617))
- JWT.io, un recurso útil para aprender más sobre JWTs ([enlace](https://jwt.io/))
- OAuth 2.0, un estándar para autorización ([enlace](https://oauth.net/2/))