---
title:                "Swift: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por qué enviar una solicitud HTTP es importante

Enviar una solicitud HTTP es una parte esencial de la programación Swift y permite a los desarrolladores interactuar con servidores y realizar diversas acciones en línea, como obtener datos, enviar formularios o crear usuarios.

## Cómo hacerlo

Para enviar una solicitud HTTP en Swift, primero debemos importar la biblioteca `Foundation` y crear una instancia de `URLSession` para manejar la comunicación con el servidor. Luego, podemos utilizar la función `dataTask` para enviar la solicitud y recibir la respuesta:

```Swift
import Foundation

let session = URLSession.shared

if let url = URL(string: "https://ejemplo.com/api/usuarios") {
    // Crear solicitud POST con un cuerpo de datos
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.httpBody = "nombre=Juan&apellido=Pérez".data(using: .utf8)
    
    // Enviar la solicitud y manejar la respuesta
    let task = session.dataTask(with: request) { data, response, error in
        if let data = data {
            // Convertir la respuesta en una cadena JSON
            let json = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]
            if let datos = json?["datos"] as? [String: Any] {
                // Imprimir el ID del usuario creado
                print("ID del usuario: \(datos["id"])")
            }
        }
    }
    // Iniciar la tarea
    task.resume()
}
```

La respuesta del servidor nos llegará en forma de datos, que podemos convertir en una cadena JSON y extraer la información que necesitamos.

## Profundizando

Al enviar una solicitud HTTP, debemos tener en cuenta algunos aspectos importantes. Primero, debemos asegurarnos de utilizar el método HTTP correcto para la acción deseada, como GET para obtener datos, POST para enviar información o DELETE para eliminar recursos. Además, es importante incluir las cabeceras adecuadas en la solicitud, ya que pueden afectar la forma en que el servidor maneja nuestros datos.

También es importante manejar los posibles errores que puedan surgir al enviar la solicitud, como problemas de conexión a internet o respuestas de servidor inesperadas.

# Ver también

- [Documentación de URLSession en Swift](https://developer.apple.com/documentation/foundation/urlsession)
- [Guía para enviar solicitudes HTTP en Swift](https://www.hackingwithswift.com/example-code/networking/how-to-send-a-simple-request-to-a-url)