---
title:                "Enviando una solicitud http"
html_title:           "Swift: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP?

Enviar una solicitud HTTP puede ser útil para comunicarse con servidores externos y obtener información en tiempo real. También permite a las aplicaciones interactuar con APIs y servicios web para realizar acciones como enviar o recibir datos.

## Cómo hacerlo

Para enviar una solicitud HTTP en Swift, se puede utilizar la clase `URLSession` y su método `dataTask(with:completionHandler:)`. A continuación, se muestra un ejemplo de cómo enviar una solicitud GET a una URL y obtener la respuesta en formato JSON:

```swift
// Crear la URL de la solicitud
let url = URL(string: "https://ejemplo.com/api/usuarios")

// Crear la solicitud con el método GET
var solicitud = URLRequest(url: url!)
solicitud.httpMethod = "GET"

// Crear una sesión y una tarea de datos
let sesion = URLSession.shared
let tarea = sesion.dataTask(with: solicitud) { datos, respuesta, error in
    // Manejar la respuesta del servidor
    guard let datos = datos else { return }
    do {
        // Convertir los datos en formato JSON
        let respuestaJSON = try JSONSerialization.jsonObject(with: datos, options: [])
        print(respuestaJSON)
    } catch {
        print("Error al convertir los datos en formato JSON")
    }
}
// Iniciar la tarea
tarea.resume()
```

La respuesta obtenida será un diccionario con la información en formato JSON. Por ejemplo:

```swift
["nombre": "Juan", "apellido": "Pérez"]
```

Para enviar una solicitud con un método diferente a GET, se debe cambiar el `httpMethod` en la solicitud y añadir los datos a enviar en el cuerpo de la misma.

## Profundizando

Las solicitudes HTTP pueden incluir distintos tipos de encabezados, como `Content-Type` o `Authorization`, que permiten enviar información adicional al servidor. Además, se pueden enviar datos en formato JSON en el cuerpo de la solicitud para realizar acciones como crear o actualizar recursos en un servidor.

Otras clases útiles para trabajar con solicitudes HTTP en Swift son `URLComponents` para manejar la URL de forma sencilla y `URLRequest` para personalizar la solicitud con encabezados, métodos, entre otros.

## Ver también

- [Documentación de URLSession en Swift](https://developer.apple.com/documentation/foundation/urlsession)
- [Tutorial de Request HTTP en Swift](https://www.hackingwithswift.com/articles/118/urlsession-tutorial-using-nsurlsession)
- [Ejemplo de uso de URLSession en una aplicación iOS](https://www.iosapptemplates.com/blog/swift-programming/urlsession-ios-swift)