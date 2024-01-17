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

## ¿Qué y Por qué?

Enviar una solicitud HTTP con autenticación básica es una forma de asegurar que un cliente tenga acceso a un recurso en un servidor mediante el uso de un nombre de usuario y contraseña. Los programadores lo hacen para garantizar que solo los usuarios autorizados puedan acceder a ciertas partes de una aplicación o sitio web.

## ¿Cómo hacerlo?

Para enviar una solicitud HTTP con autenticación básica en Swift, primero debes importar la biblioteca `URLSession`. A continuación, puedes construir tu URL y crear una solicitud `URLRequest` especificando el método HTTP que deseas utilizar, la URL y las credenciales de autenticación. Por ejemplo:

```
import URLSession

let url = URL(string: "https://api.example.com")
var request = URLRequest(url: url!)
request.httpMethod = "GET"
let username = "usuario"
let password = "contraseña"
let loginString = "\(username):\(password)"
let loginData = loginString.data(using: .utf8)
let base64LoginString = loginData!.base64EncodedString()
request.setValue("Basic \(base64LoginString)", forHTTPHeaderField: "Authorization")

let task = URLSession.shared.dataTask(with: request) { data, response, error in
  guard let data = data, error == nil else {                                                  
    print("Error al enviar solicitud: \(error!)")                                             
    return
  }                                             
  if let httpStatus = response as? HTTPURLResponse, httpStatus.statusCode != 200 {           
    print("Estado de la respuesta: \(httpStatus.statusCode)")                                
  }                                             
  let responseString = String(data: data, encoding: .utf8)                                          
  print("Respuesta: \(responseString!)")                                                                     
}                                             
task.resume()
```

Al ejecutar esta solicitud, si los datos de autenticación son correctos, recibirás una respuesta HTTP exitosa (código 200) y podrás acceder al recurso solicitado.

## Mirando más profundo

La autenticación básica HTTP ha sido una forma común de autenticación en la web desde mediados de los años 90. Sin embargo, ha sido sustituida por métodos más seguros, como la autenticación de tokens y OAuth. Aunque sigue siendo ampliamente compatible, se recomienda utilizar otros métodos de autenticación para garantizar una mayor seguridad.

## Ver también

Puedes obtener más información sobre la autenticación básica HTTP en la [documentación oficial de Apple] (https://developer.apple.com/documentation/foundation/urlsession). También puedes leer más sobre los diferentes métodos de autenticación en la web en [este artículo de MDN] (https://developer.mozilla.org/es/docs/Web/HTTP/Authentication).