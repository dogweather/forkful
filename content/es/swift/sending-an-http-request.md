---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviando una petición HTTP con Swift

## ¿Qué y Por qué?

Enviando una petición HTTP es cómo nuestros programas hablan con servidores web. Lo hacemos para recuperar o enviar datos a través de la web.

## Cómo hacerlo:

En Swift, puedes enviar una solicitud HTTP usando `URLSession`. Aquí hay un ejemplo simple:

```Swift
import Foundation

let url = URL(string: "https://api.misitio.com")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        print(String(data: data, encoding: .utf8)!)
    }
}
task.resume()
```

Al ejecutar el código anterior, harás una petición GET a `https://api.misitio.com` y imprimirás la respuesta.

## Profundizando

En el pasado, enviar una solicitud HTTP era un proceso mucho más complicado. Pero con el tiempo y las actualizaciones de software, Swift ha hecho que este proceso sea mucho más sencillo.

A pesar de eso, hay alternativas a `URLSession`, como Alamofire, que pueden ofrecer más funcionalidades. Sin embargo, `URLSession` sigue siendo una excelente opción debido a su simplicidad e integración nativa con Swift.

Cuando envías una petición HTTP con `URLSession`, ocurren una serie de etapas: la creación de la URL, la construcción de la tarea usando la sesión compartida, el establecimiento del manejador de completado y, finalmente, la reanudación de la tarea.

## Ver También

Para aprender más acerca de las peticiones HTTP y `URLSession`, revisa los siguientes recursos:

- Documentación oficial de Apple sobre URLSession: [URLSession - Foundation | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- Alamofire, una alternativa a URLSession: [Alamofire GitHub](https://github.com/Alamofire/Alamofire)
- TutorialsPoint sobre peticiones HTTP: [TutorialsPoint HTTP](https://www.tutorialspoint.com/http/index.htm)