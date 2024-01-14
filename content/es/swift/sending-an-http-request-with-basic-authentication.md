---
title:                "Swift: Enviando una petición http con autenticación básica"
simple_title:         "Enviando una petición http con autenticación básica"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, es común necesitar comunicarse con servidores para obtener información o realizar acciones. En estos casos, enviar una solicitud HTTP con autenticación básica es una manera segura y confiable de hacerlo. Aprender cómo hacerlo es una habilidad valiosa para cualquier programador.

## Cómo hacerlo

En Swift, podemos enviar una solicitud HTTP con autenticación básica utilizando la clase `URLRequest` y su método `addValue` para añadir encabezados de autenticación. A continuación, podemos utilizar la clase `URLSession` para ejecutar la solicitud y obtener una respuesta. Veamos un ejemplo:

```Swift
let urlString = "https://www.example.com/api/users"
let url = URL(string: urlString)
let username = "usuario"
let password = "contraseña"

if let requestUrl = url {
    var request = URLRequest(url: requestUrl)
    request.httpMethod = "GET"
    let loginString = "\(username):\(password)"
    let loginData = loginString.data(using: .utf8)
    let base64LoginString = loginData?.base64EncodedString()
    request.addValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

    let session = URLSession.shared
    let task = session.dataTask(with: request) { (data, response, error) in
        if let error = error {
            print("Error: \(error)")
        } else if let data = data, let response = response as? HTTPURLResponse, response.statusCode == 200 {
            print("Respuesta del servidor: \(data)")
        }
    }
    task.resume()
}
```

Este código envía una solicitud GET al servidor utilizando la autenticación básica. En la línea 9, creamos un encabezado de autenticación utilizando el nombre de usuario y contraseña proporcionados, y lo añadimos a la solicitud en la línea 10. Después de ejecutar la solicitud, podemos obtener la respuesta y manejarla en el bloque de finalización del `dataTask` en las líneas 14-19. Aquí, simplemente imprimimos la respuesta del servidor si la solicitud es exitosa.

## Profundizando

Cuando utilizamos la autenticación básica, los datos de inicio de sesión se envían en formato de texto simple, lo que hace que sea fácil para un atacante interceptar y leer la información. Por lo tanto, si estamos enviando información sensible, es importante asegurarse de que la conexión sea segura mediante la utilización de HTTPS. También es recomendable utilizar otros métodos de autenticación más seguros, como OAuth.

## Vea también

- [Documentación oficial de Apple para URLRequest](https://developer.apple.com/documentation/foundation/urlrequest)
- [Tutorial sobre cómo enviar solicitudes HTTP en Swift](https://medium.com/swift-programming/basics-of-http-request-in-swift-c07f5f72903)
- [Información sobre seguridad en conexiones HTTP](https://www.w3.org/Security/wiki/SecureConnections)