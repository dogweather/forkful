---
title:                "Leyendo un archivo de texto"
html_title:           "Swift: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer un archivo de texto es una acción común que los programadores realizan con frecuencia. Consiste en leer y obtener información almacenada en un archivo de texto plano, que puede contener cualquier tipo de texto sin formato, como palabras o datos numéricos. Los programadores lo hacen para acceder a esta información y utilizarla en sus programas.

## Cómo:

```Swift
// Ejemplo de código para leer un archivo de texto
if let path = Bundle.main.path(forResource: "ejemplo", ofType: "txt") {
    do {
        // Lee el contenido del archivo como una cadena de texto
        let contenido = try String(contentsOfFile: path, encoding: .utf8)
        print(contenido)
    } catch {
        // Error al leer el archivo
        print("No se pudo leer el archivo")
    }
}
```

El resultado de este código sería imprimir en la consola el contenido del archivo de texto "ejemplo.txt".

## Profundizando:

Leer archivos de texto ha sido una técnica utilizada durante décadas por los programadores. Fue una forma común de almacenar y compartir información antes de la llegada de las bases de datos y aplicaciones de mensajería instantánea. Aunque existen otras formas de almacenar y manipular datos, aun se utiliza la lectura de archivos de texto en situaciones donde se requiere un formato simple y legible.

Una alternativa a la lectura de archivos de texto es el uso de bases de datos, donde la información se organiza de manera estructurada y se puede acceder y actualizar fácilmente. Sin embargo, en ciertos casos, como en la transferencia de datos entre diferentes sistemas, la lectura de archivos de texto sigue siendo una opción viable.

En cuanto a la implementación, existen diferentes métodos y librerías en Swift que permiten leer archivos de texto, como el ejemplo que se mostró arriba utilizando la clase `String`. Además, también se pueden utilizar otras clases como `Data`, `Scanner` o `FileManager` para leer y manejar archivos de texto de diferentes maneras.

## Ver También:

- [Documentación oficial de Swift - Manipulación de archivos](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial de lectura y escritura de archivos en Swift](https://www.ralfebert.de/ios/tutorials/ios-swift-reading-writing-files/)
- [Ejemplo práctico de lectura de archivos de texto en Swift](https://www.hackingwithswift.com/example-code/system/how-to-load-a-string-from-a-file-in-your-bundle)