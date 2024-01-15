---
title:                "Descargando una página web"
html_title:           "Swift: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web es una tarea común en el desarrollo de aplicaciones y sitios web. Puede ser necesario para acceder a ciertos datos o para mostrar información específica en la pantalla. En este artículo, aprenderemos cómo descargar una página web utilizando el lenguaje de programación Swift.

## Cómo hacerlo

Para descargar una página web en Swift, utilizaremos la clase `URLSession`. Esta clase nos permitirá hacer peticiones HTTP y recibir los datos de respuesta. Aquí hay un ejemplo de cómo descargar una página web y mostrar su contenido en la consola:

```Swift
if let url = URL(string: "https://example.com") {
    
    // Creamos una sesión con una configuración predeterminada
    let session = URLSession(configuration: .default)
    
    // Creamos una tarea de datos utilizando la URL
    let task = session.dataTask(with: url) { (data, response, error) in
        
        // Verificamos si hay algún dato de respuesta y si no hay ningún error
        if let data = data, error == nil {
            
            // Convertimos los datos recibidos a una cadena de texto legible
            if let pageContent = String(data: data, encoding: .utf8) {
                
                // Imprimimos el contenido de la página en la consola
                print(pageContent)
            }
        }
    }
    
    // Iniciamos la tarea
    task.resume()
}
```

Este código nos ayudará a descargar la página web y almacenar su contenido en una variable de tipo `String`. Luego, podemos utilizar esa variable para mostrar el contenido en cualquier parte de nuestra aplicación.

## Un vistazo más profundo

Si queremos ser más avanzados en nuestra solicitud de descarga, podemos personalizar nuestra sesión utilizando la clase `URLSessionConfiguration`. Por ejemplo, podemos especificar el tiempo de espera de la solicitud o agregar encabezados personalizados a la petición.

También podemos utilizar diferentes tipos de tareas para descargar una página web, como `downloadTask` para descargar el contenido de una URL en un archivo local.

En resumen, la clase `URLSession` nos permite tener un mayor control sobre las solicitudes HTTP en nuestra aplicación, lo que facilita la descarga de páginas web y otro tipo de contenidos.

## Ver también

- [Documentación oficial de Apple: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Tutorial: Descargar y mostrar imágenes en Swift](https://www.hackingwithswift.com/example-code/libraries/how-to-download-and-parse-json)
- [Libro: Swift: Desarrollo de aplicaciones para iOS de manera avanzada](https://www.amazon.es/Swift-Desarrollo-aplicaciones-avanzada-Crash/dp/8426725578)