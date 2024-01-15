---
title:                "Extrayendo subcadenas"
html_title:           "Swift: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué extraer subcadenas en Swift

Extraer subcadenas, o partes de una cadena de texto, es una habilidad útil en programación. Puede ser necesaria para tareas como filtrar datos o manipular strings antes de presentarlos al usuario. Afortunadamente, Swift ofrece una forma sencilla de hacerlo con su método `subString()`.

## Cómo hacerlo

Para extraer una subcadena en Swift, usaremos el método `subString()` seguido de los índices que delimitan la parte de la cadena que queremos extraer. Por ejemplo, si tenemos una cadena "¡Hola mundo!" y queremos extraer la palabra "mundo", podemos usar `subString()` de la siguiente manera:

```
let str = "¡Hola mundo!"
let indexInicio = str.index(str.startIndex, offsetBy: 6) // índice 6 = "m"
let indexFin = str.index(str.endIndex, offsetBy: -1) // tomamos la posición anterior al índice de final de la cadena
let subcadena = str.substring(with: indexInicio...indexFin) // "mundo"
```
En este ejemplo, usamos `index()` para obtener los índices que delimitan la palabra "mundo" y luego los pasamos como argumentos al método `subString()`. Al imprimir `subcadena`, obtenemos el resultado deseado.

## Un poco más profundo

Además de usar índices específicos, también podemos usar el método `range()` para definir una subcadena. Por ejemplo, si queremos extraer la palabra "Hola" de nuestra cadena anterior, podemos hacerlo de la siguiente manera:

```
let str = "¡Hola mundo!"
let rango = str.range(of: "Hola")! // el signo de exclamación se usa para desempaquetar el opcional devuelto por range()
let subcadena = str.substring(with: rango) // "Hola"
```

También podemos usar `range()` para aplicar filtros a la cadena. Por ejemplo, si queremos extraer todas las palabras que empiezan con "H", podemos hacerlo de la siguiente manera:

```
let str = "¡Hola mundo! Hice Hamburguesas y Hombres Felices"
let searchOptions: CompareOptions = [.anchored, .caseInsensitive]
// el primer parámetro de range() indica la palabra que buscamos, el parámetro options indica que queremos buscar ignorando mayúsculas y minúsculas (no case-sensitive) y que la búsqueda esté al principio de la cadena (anchored)
for palabra in str.components(separatedBy: " ") { // separamos la cadena en partes por cada espacio
    if let range = palabra.range(of: "H", options: searchOptions) {
        print(str.substring(with: range))
    }
}
// salida: "Hola", "Hice", "Hamburguesas", "Hombres"

```

## Ver también

- [Documentación oficial de Swift sobre `subString()`](https://developer.apple.com/documentation/swift/string/1540112-substring)
- [Otras formas de trabajar con strings en Swift](https://code.tutsplus.com/es/tutorials/working-with-strings-in-swift-3--cms-26508)
- [Más información sobre el tipo de dato `String` en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)