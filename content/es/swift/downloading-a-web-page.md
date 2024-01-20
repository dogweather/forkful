---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Descargar una página web se refiere a conseguir su código HTML. Los programadores lo hacen para recuperar y analizar datos, o para hacer copias de seguridad de la web.

## Cómo hacerlo
En Swift, generamos una solicitud del sitio web y luego utilizamos una tarea de URL para descargarla. Aquí te dejo un ejemplo en Swift 5:

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!
let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
    if let data = data {
        let str = String(data: data, encoding: .utf8)
        print("Datos de la página:\n\(str!)")
    }
}
task.resume()
```

Este programa imprimirá el código HTML de `www.example.com`.

## Buceo profundo
La descarga de páginas web ha sido fundamental en la vida útil de la web, permitiendo la indexación de motores de búsqueda y la toma de instantáneas de sitios. Además del método anterior, podrías usar bibliotecas como Alamofire para tareas más complejas. Cabe mencionar que si haces demasiadas solicitudes a un sitio en un corto período de tiempo, puedes ser bloqueado.

## Ver también
2. [Alamofire](https://github.com/Alamofire/Alamofire): Una biblioteca Swift para tareas HTTP más complejas.
3. [Documentación oficial de Swift](https://developer.apple.com/documentation/swift): Para profundizar en el lenguaje de programación Swift.