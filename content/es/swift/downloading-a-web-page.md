---
title:                "Descargar una página web."
html_title:           "Swift: Descargar una página web."
simple_title:         "Descargar una página web."
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Descargar una página web es simplemente obtener el código HTML de una página de Internet y guardarlo en tu dispositivo. Los programadores hacen esto para acceder a la información de una página, como imágenes, texto o datos, para utilizarla en sus propias aplicaciones o herramientas.

## ¿Cómo hacerlo?
```
Swift
import UIKit
import Foundation

// Definir la URL de la página web que queremos descargar
let url = URL(string: "https://google.com")!

// Crear una sesión de URLRequest
let request = URLRequest(url: url)

// Realizar la solicitud de descarga
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    // Manejar los datos recibidos
    if let data = data {
        // Convertir los datos a una cadena de texto
        let html = String(data: data, encoding: .utf8)
        print(html)
    }
}
// Iniciar la tarea
task.resume()
```

## Profundizando
Descargar una página web se ha vuelto una habilidad esencial para los programadores modernos, ya que nos permite utilizar información de diversas fuentes en nuestras aplicaciones. Además de utilizar la URLSession en Swift, hay otras formas de descargar una página web, como utilizar librerías de terceros o incluso hacerlo manualmente con sockets.

## Ver también
- [Apple Developer: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Swifts: Getting Started with Network Requests](https://www.hackingwithswift.com/read/32/1/introduction)
- [URLSession Class Reference](https://developer.apple.com/documentation/foundation/urlsession)