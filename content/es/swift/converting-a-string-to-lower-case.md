---
title:                "Convirtiendo un string a minúsculas"
html_title:           "Swift: Convirtiendo un string a minúsculas"
simple_title:         "Convirtiendo un string a minúsculas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez has tenido que trabajar con palabras o frases en un programa y te has dado cuenta de que algunas están escritas en mayúsculas y otras en minúsculas? Puede ser frustrante tener que lidiar con esta inconsistencia. Afortunadamente, en Swift tenemos la capacidad de convertir una cadena de texto a minúsculas, lo que nos permite uniformizar nuestros datos y facilitar su manipulación.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Swift, podemos utilizar el método `lowercased()` como se muestra en el siguiente ejemplo:

```Swift
let cadena = "Hola Mundo"
let cadenaEnMinusculas = cadena.lowercased()
print(cadenaEnMinusculas)
```

La salida de este código sería: `hola mundo`. Como se puede ver, el método `lowercased()` toma la cadena original y la convierte en una nueva cadena con todas las letras en minúsculas. Es importante destacar que este método no afecta a la cadena original, sino que crea una nueva cadena a partir de ella.

También podemos utilizar este método en conjunto con otras funciones de manipulación de cadenas, como por ejemplo:

```Swift
let nombre = "Juan"
let apellido = "García"
let nombreCompleto = "\(nombre.lowercased()) \(apellido.lowercased())"
print(nombreCompleto)
```

En este caso, la salida sería `juan garcía`. Aquí vemos cómo podemos utilizar el método `lowercased()` para convertir cada parte del nombre en minúsculas antes de concatenarlas en una sola cadena.

## Profundizando

El método `lowercased()` es una propiedad de la clase `String` en Swift y es parte de la biblioteca estándar del lenguaje. Esto significa que podemos utilizarlo en cualquier cadena sin necesidad de importar librerías externas.

Además de `lowercased()`, también tenemos el método `uppercased()` que convierte una cadena a mayúsculas y `capitalized()` que capitaliza la primera letra de cada palabra en la cadena. Estos métodos nos permiten tener un mayor control sobre cómo se muestran los datos en nuestra aplicación.

Es importante mencionar que estos métodos utilizan las reglas de capitalización del idioma en el que se está corriendo el programa, lo que significa que si estamos trabajando en un programa en español, la cadena será convertida de acuerdo a las reglas de este idioma.

## Ver también

- [Documentación oficial de Swift sobre la clase String](https://developer.apple.com/documentation/swift/string)
- [Tutorial sobre manipulación de cadenas en Swift](https://www.raywenderlich.com/5008164-swift-string-formatting-and-manipulation-cheat-sheet)