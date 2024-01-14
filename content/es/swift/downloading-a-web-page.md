---
title:                "Swift: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué
Descargar páginas web puede ser útil para desarrolladores que desean obtener información de una página, extraer datos o simplemente para fines de análisis.

## Cómo Hacerlo
La descarga de páginas web en Swift es fácil y se puede hacer utilizando la clase `URLSession`. Primero, debemos crear una URL que apunte a la página que queremos descargar. Luego, creamos una instancia de `URLSession` y usamos el método `dataTask` para crear una tarea de solicitud de datos. Finalmente, manejamos la respuesta de la tarea utilizando un bloque de finalización y obtenemos los datos descargados.

```
let url = URL(string: "https://www.example.com")!
let session = URLSession.shared

session.dataTask(with: url) { (data, response, error) in
    if let data = data {
        let html = String(data: data, encoding: .utf8)
        print(html) // Esto imprimirá el código HTML de la página descargada
    }
}.resume()
```

## Profundizando
Para aquellos que deseen tener un control más específico sobre la descarga de la página, también hay opciones como especificar el método de solicitud HTTP, agregar encabezados y establecer parámetros en la URL. También es importante tener en cuenta la seguridad al descargar páginas web, ya que se pueden encontrar páginas maliciosas.

## Ver también
- [Documentación de Apple sobre URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [Tutorial de descarga de páginas web en Swift](https://www.ralfebert.de/ios/tutorials/ios-swift-nsurlconnection-or-urlsession-download/)
- [Uso de bloques de finalización en Swift](https://docs.swift.org/swift-book/LanguageGuide/Closures.html)