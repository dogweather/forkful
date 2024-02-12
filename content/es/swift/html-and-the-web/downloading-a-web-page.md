---
title:                "Descargando una página web"
aliases:
- /es/swift/downloading-a-web-page.md
date:                  2024-01-20T17:44:50.561381-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?
Descargar una página web es traer el contenido de una URL a tu aplicación. Lo hacemos para procesar, mostrar o analizar datos en nuestras apps.

## Cómo hacerlo:
Primero, asegúrate de que tienes permisos para acceder a la red en tu `Info.plist`. Ahora, con Swift y URLSession es fácil:

```Swift
import Foundation

let url = URL(string: "https://example.com")!

let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Error al descargar la página: \(error)")
        return
    }
    
    guard let httpResponse = response as? HTTPURLResponse, (200...299).contains(httpResponse.statusCode) else {
        print("Error en la respuesta del servidor")
        return
    }

    if let mimeType = httpResponse.mimeType, mimeType == "text/html",
       let data = data,
       let string = String(data: data, encoding: .utf8) {
        print("Página descargada: \(string)")
    }
}

task.resume()
```

Si todo va bien, deberías ver el HTML de la página impreso en la consola.

## Inmersión Profunda:
Antes de Swift y URLSession, descargar una página web se hacía comúnmente con APIs como NSURLConnection, que ahora están obsoletas. URLSession es más flexible y fácil de usar.

NSURLConnection (obsoleto) ➡︎ URLSession.

Alternativas a URLSession incluyen librerías de terceros como Alamofire, que ofrecen más funcionalidades y simplifican tareas comunes.

NSURLSession maneja los detalles de implementación internos como la gestión de la conexión de red, pero es importante manejar los errores y verificar el estado del código HTTP para entender cómo fue la petición.

## Vea También:
- Documentación oficial de URLSession: [URLSession Apple Developer](https://developer.apple.com/documentation/foundation/urlsession)
- Alamofire, una librería HTTP de terceros para Swift: [Alamofire GitHub](https://github.com/Alamofire/Alamofire)
- Guía para manejo de permisos de red en Info.plist: [Info.plist Permissions](https://developer.apple.com/documentation/bundleresources/information_property_list)
