---
date: 2024-01-20 18:00:50.846860-07:00
description: "C\xF3mo hacerlo: Swift facilita el trabajo con solicitudes HTTP. Aqu\xED\
  \ tienes un ejemplo usando `URLSession` para hacer una solicitud GET simple."
lastmod: '2024-03-13T22:44:59.413417-06:00'
model: gpt-4-1106-preview
summary: Swift facilita el trabajo con solicitudes HTTP.
title: Enviando una solicitud http
weight: 44
---

## Cómo hacerlo:
Swift facilita el trabajo con solicitudes HTTP. Aquí tienes un ejemplo usando `URLSession` para hacer una solicitud GET simple.

```Swift
import Foundation

// Creamos la URL
if let url = URL(string: "https://api.example.com/data") {
    // Configuramos el URLSession
    let session = URLSession(configuration: .default)
    
    // Creamos la solicitud
    let task = session.dataTask(with: url) { data, response, error in
        if let error = error {
            // Manejamos el error aquí
            print("Error al realizar la solicitud: \(error)")
            return
        }
        
        // Aseguramos que tenemos una respuesta válida y datos
        if let httpResponse = response as? HTTPURLResponse, httpResponse.statusCode == 200, let data = data {
            // Aquí trabajamos con los datos recibidos
            if let stringData = String(data: data, encoding: .utf8) {
                // Imprimimos la respuesta en formato String
                print(stringData)
            }
        } else {
            print("No se recibieron datos válidos")
        }
    }
    
    // Iniciamos la tarea
    task.resume()
}
```

## Deep Dive:
Enviar solicitudes HTTP es esencial para casi todas las aplicaciones conectadas a internet. Antes de `URLSession` en Swift, los desarrolladores usaban `NSURLConnection` pero hoy `URLSession` es la opción estándar debido a su amplia gama de funcionalidades y su mayor eficiencia.

Si necesitamos más control o dependemos de comportamientos específicos, podemos usar librerías de terceros como Alamofire. Cuando trabajamos a bajo nivel, podríamos lidiar con `CFNetwork` API en iOS, que brinda una granularidad aún más detallada.

Detrás de cada solicitud HTTP hay una serie de pasos: DNS lookup, TCP handshake, envío de la solicitud HTTP y la espera de la respuesta del servidor, que también puede ser analizado para manejar códigos de respuesta, cabeceras y cookies.

## Ver También:
- Documentación oficial de URLSession: https://developer.apple.com/documentation/foundation/urlsession
- Alamofire, una librería HTTP de networking para Swift: https://github.com/Alamofire/Alamofire
- Guía de red para desarrolladores iOS: https://developer.apple.com/library/archive/documentation/NetworkingInternetWeb/Conceptual/NetworkingOverview/Introduction/Introduction.html
- Tutorial de Ray Wenderlich sobre `URLSession`: https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started
