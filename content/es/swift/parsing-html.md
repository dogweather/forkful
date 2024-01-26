---
title:                "Análisis de HTML"
date:                  2024-01-20T15:34:07.974323-07:00
html_title:           "Arduino: Análisis de HTML"
simple_title:         "Análisis de HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
El análisis (parsing) de HTML consiste en transformar el código HTML en estructuras de datos que Swift puede manejar. Lo hacemos para extraer información, manipularla o incluso alterar el HTML de manera programática.

## Cómo hacerlo:
```Swift
import Foundation
import SwiftSoup

let htmlString = "<html><head><title>Mi página</title></head><body><p>Hola, mundo!</p></body></html>"

do {
    let doc: Document = try SwiftSoup.parse(htmlString)
    let bodyText: Elements = try doc.select("p")
    if let text = try bodyText.first()?.text() {
        print(text) // Salida: Hola, mundo!
    }
} catch Exception.Error(let type, let message) {
    print("Error de tipo \(type) con mensaje: \(message)")
} catch {
    print("Otro error")
}
```

## Análisis Profundo
En los inicios de la web, los navegadores eran menos estrictos con el HTML, haciendo más complicado el parsing correcto. Hoy en día, librerías como SwiftSoup se basan en estándares bien definidos. Alternativas a SwiftSoup podrían ser librerías basadas en NSXMLParser de Apple pero carecen de la simplicidad de SwiftSoup para trabajar con HTML. Al implementar parsing de HTML, es importante manejar errores, ya que el HTML malformado puede causar excepciones inesperadas.

## Ver También
- [SwiftSoup Github Repository](https://github.com/scinfu/SwiftSoup)
- [World Wide Web Consortium (W3C) HTML Standards](https://www.w3.org/TR/html52/)
- Documentación de [NSXMLParser](https://developer.apple.com/documentation/foundation/nsxmlparser)
