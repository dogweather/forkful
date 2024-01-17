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

## ¿Qué y por qué?

La extracción de subcadenas es simplemente el proceso de obtener un pedazo más pequeño de una cadena de texto más grande en un programa Swift. Los programadores a menudo realizan esta tarea para manipular cadenas de texto y realizar operaciones específicas, como búsqueda o reemplazo de ciertos caracteres.

## Cómo hacerlo:

Puedes extraer una subcadena en Swift utilizando el método `subtring`. Esta función toma dos argumentos: un índice inicial y un índice final. Por ejemplo, si queremos extraer los primeros tres caracteres de una cadena, podríamos utilizar el siguiente código:

```Swift
let cadena = "Hola mundo"
let subcadena = cadena.substring(from: cadena.startIndex, to: cadena.index(cadena.startIndex, offsetBy: 3))
print(subcadena) // salida: Hol
```

También podemos usar el operador `..<` para especificar el rango de índices. Por ejemplo:

```Swift
let cadena = "Hola mundo"
let subcadena = cadena.substring(from: cadena.startIndex ..< cadena.index(cadena.startIndex, offsetBy: 3))
print(subcadena) // salida: Hol
```

Otra manera de extraer una subcadena es utilizando el método `prefix` o `suffix`, que toman como argumento la cantidad de caracteres que queremos extraer desde el inicio o el final de la cadena, respectivamente:

```Swift
let cadena = "Hola mundo"
let subcadena1 = cadena.prefix(3)
print(subcadena1) // salida: Hol

let subcadena2 = cadena.suffix(4)
print(subcadena2) // salida: undo
```

## Profundizando:

La extracción de subcadenas ha sido una tarea común en la programación desde los primeros lenguajes de programación. Sin embargo, en Swift, los desarrolladores a menudo prefieren utilizar cadenas de caracteres (o `String` en inglés). Esto se debe a que las cadenas de caracteres en Swift son más potentes, ya que ofrecen una variedad de métodos para manipular y extraer subcadenas. Además, Swift también tiene la función `substring(with:)` que permite especificar un rango de índices más fácilmente.

Alternativamente, también se podría utilizar la función `NSString.substring(with:)` para extraer subcadenas en Swift, ya que `String` es compatible con `NSString`. Sin embargo, esta opción no es tan eficiente como el uso de métodos nativos de `String`, por lo que se recomienda utilizar los métodos mencionados anteriormente.

## Ver también:

Para más información sobre cadenas de texto y cómo trabajar con ellas en Swift, puedes consultar la documentación oficial de Apple sobre `String` y `Substring`:

[String (Apple Documentation)](https://developer.apple.com/documentation/swift/string)

[Substring (Apple Documentation)](https://developer.apple.com/documentation/swift/substring)

También puedes explorar el repositorio de Swift en GitHub para encontrar ejemplos de código y más recursos sobre este tema:

[Swift Repository on GitHub](https://github.com/apple/swift/tree/main/stdlib/public/core)