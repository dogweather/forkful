---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Swift: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué
¿Por qué alguien querría enviar una solicitud HTTP con autenticación básica? La autenticación básica agrega una capa adicional de seguridad a los sitios web y aplicaciones al requerir un nombre de usuario y contraseña para acceder a ciertas funciones o datos sensibles.

## Cómo hacerlo
La autenticación básica en Swift se puede lograr fácilmente al enviar un encabezado de autorización en la solicitud HTTP. Aquí hay un ejemplo de cómo hacerlo utilizando URLSession:

```Swift
let username = "usuario"
let password = "contraseña"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
if let base64EncodedString = loginData?.base64EncodedString() {
    let request = URLRequest(url: URL(string: "https://ejemplo.com/login")!)
    request.setValue("Basic \(base64EncodedString)", forHTTPHeaderField: "Authorization")
    let task = URLSession.shared.dataTask(with: request) { data, response, error in
        if let error = error {
            print("Error: \(error)")
        } else if let data = data, let string = String(data: data, encoding: .utf8) {
            print("Respuesta: \(string)")
        }
    }
    task.resume()
}
```

El código anterior mostrará cómo enviar una solicitud HTTP con autenticación básica utilizando una combinación de un nombre de usuario y una contraseña. La respuesta será la respuesta de la solicitud, que puede ser una confirmación de inicio de sesión exitoso o un mensaje de error.

## Profundizando
Al utilizar la autenticación básica, se recomienda utilizar HTTPS en lugar de HTTP para una mayor seguridad. Además, la autenticación básica transfiere la información de inicio de sesión en texto sin formato, por lo que no se recomienda para aplicaciones que manejan datos muy sensibles. En su lugar, se pueden utilizar métodos de autenticación más seguros, como OAuth.

## Vea también
- [Documentación oficial de Apple sobre URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Artículo en Medium sobre autenticación básica en Swift](https://medium.com/@YogevSitton/use-basic-authentication-in-swift-3-0-e9079d99384a)
- [Guía sobre OAuth en Swift](https://www.raywenderlich.com/9272-how-to-implement-oauth-2-0-in-swift)