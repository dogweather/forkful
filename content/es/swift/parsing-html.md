---
title:                "Análisis sintáctico de html"
html_title:           "Ruby: Análisis sintáctico de html"
simple_title:         "Análisis sintáctico de html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El análisis (parsing) de HTML implica descomponer estructuralmente el lenguaje de marcado para otras operaciones y análisis. Los programadores lo hacen para manipular, extraer o modificar contenido dentro de páginas HTML.

## Cómo se hace:

Veamos cómo podemos analizar HTML en Swift usando la biblioteca SwiftSoup. 

Primero, debemos instalar la biblioteca SwiftSoup. 
```
pod 'SwiftSoup'
```

Ahora, aquí hay un ejemplo de cómo podemos analizar HTML.
```Swift
import SwiftSoup

let html = "<html><head><title>Primer parsing</title></head><body><p>Post body<p></body></html>"
do {
    let doc: Document = try SwiftSoup.parse(html)
    let title: Elements = try doc.select("title")
    print(title.text()) //Devuelve: Primer parsing
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("error")
}
```
En este ejemplo, hemos seleccionado y extraído la etiqueta "title" del código HTML.

## Inmersión Profunda

Históricamente, el parsing de HTML se ha realizado utilizando diversas bibliotecas y lenguajes. Sin embargo, SwiftSoup, una alternativa liviana, clara y fácil de usar, se ha convertido en una elección popular en Swift.

Existen alternativas a SwiftSoup, como Kanna y Ji, pero SwiftSoup se destaca por su simplicidad y ausencia de dependencias de terceros.

Una de las particularidades de la implementación de SwiftSoup es que se basa en el análisis jerárquico del DOM, lo que permite a los programadores seleccionar y manipular fácilmente las partes del documento HTML.

## Consulte También

Aquí hay algunas fuentes adicionales que pueden ser útiles:

1. Documentación de SwiftSoup: [https://tid-kijyun.github.io/Kanna/](https://tid-kijyun.github.io/Kanna/)
2. Uso avanzado de SwiftSoup: [https://medium.com/@tiborbodecs/swiftsoup-html-parsing-in-swift-35fb26e216c](https://medium.com/@tiborbodecs/swiftsoup-html-parsing-in-swift-35fb26e216c)
3. Documentación oficial de Swift: [https://developer.apple.com/swift/](https://developer.apple.com/swift/)