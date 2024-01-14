---
title:                "Swift: Encontrando la longitud de una cadena"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías encontrar la longitud de una cadena?

En programación, a menudo necesitas trabajar con cadenas de texto. Una operación muy común es encontrar la longitud de una cadena, es decir, cuántos caracteres contiene. Esto es útil para tareas como validar inputs de usuario, contar palabras en un párrafo o simplemente para saber cuánto espacio ocupará una cadena en memoria. En esta publicación, aprenderás cómo hacerlo en Swift.

## Cómo hacerlo

Para encontrar la longitud de una cadena en Swift, podemos usar la propiedad `count` en una instancia de `String`. Por ejemplo:

```Swift
let string = "¡Hola, mundo!"
let length = string.count
print(length) // Output: 13
```

En este ejemplo, creamos una cadena con el saludo clásico "¡Hola, mundo!" y luego usamos `count` para obtener su longitud, que es 13.

También podemos encontrar la longitud de una cadena utilizando el método `countElements()`, como se muestra a continuación:

```Swift
let anotherString = "Este texto tiene 7 palabras."
let length = countElements(anotherString)
print(length) // Output: 30
```

En este caso, usamos una función de Swift para encontrar la longitud de la cadena, que también incluye los espacios y signos de puntuación.

## Profundizando

Ahora que sabemos cómo encontrar la longitud de una cadena, veamos algunos detalles más sobre este proceso. En Swift, cada carácter en una cadena, incluidos los emojis, se considera un elemento y, por lo tanto, se cuentan en la longitud de la misma.

Es importante tener en cuenta que en Swift, los emojis se pueden mostrar como uno o varios caracteres, dependiendo del sistema operativo y la fuente utilizada. Por lo tanto, la edad de una cadena que contiene emojis puede variar dependiendo del contexto en el que se muestre.

Además, mientras que en Swift 2.0 y versiones anteriores, `countElements()` también incluía los espacios en blanco en su recuento, en versiones más recientes solo cuenta los caracteres visibles. Por lo tanto, si necesitas contar los espacios en blanco, deberás usar la propiedad `count`.

## Ver también 

- [String en la documentación de Swift](https://developer.apple.com/documentation/swift/string)
- [Swift String Tutorial: Manejo de cadenas en Swift](https://www.raywenderlich.com/860-swift-string-tutorial-managing-strings-in-swift)
- [Estructuras de datos básicas de Swift](https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID106)

¡Gracias por leer y feliz codificación en Swift!