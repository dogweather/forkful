---
date: 2024-01-20 17:44:50.561381-07:00
description: "C\xF3mo hacerlo: Primero, aseg\xFArate de que tienes permisos para acceder\
  \ a la red en tu `Info.plist`. Ahora, con Swift y URLSession es f\xE1cil."
lastmod: '2024-03-13T22:44:59.415392-06:00'
model: gpt-4-1106-preview
summary: "Primero, aseg\xFArate de que tienes permisos para acceder a la red en tu\
  \ `Info.plist`."
title: "Descargando una p\xE1gina web"
weight: 42
---

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
