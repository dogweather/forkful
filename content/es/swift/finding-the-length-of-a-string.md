---
title:                "Encontrando la longitud de una cadena"
html_title:           "Swift: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, a menudo necesitamos trabajar con cadenas de texto y una de las tareas más comunes es encontrar la longitud de una cadena. Saber cómo hacerlo nos permite manipular y analizar de manera efectiva los datos en nuestras aplicaciones.

## Cómo hacerlo

En Swift, podemos encontrar la longitud de una cadena utilizando la propiedad `count` de la clase `String`. Veamos un ejemplo:

```Swift
let frase = "Hola mundo!"
print(frase.count) // Output: 12
```

Como se puede ver en el ejemplo, simplemente llamamos a la propiedad `count` en la variable que contiene la cadena y nos devuelve el número de caracteres en ella.

También podemos utilizar el método `count` en una cadena que contenga valores Unicode, como por ejemplo emojis:

```Swift
let emoji = "🚀🌎"
print(emoji.count) // Output: 2
```

Es importante tener en cuenta que la propiedad `count` devuelve el número de caracteres en la cadena, no el número de bytes. Por lo tanto, si tenemos una cadena que contiene caracteres Unicode más complejos, el resultado puede ser diferente al esperado.

## Profundizando

Si queremos entender cómo funciona la propiedad `count` en la clase `String`, podemos explorar su implementación en el código fuente de Swift. Allí veremos que utiliza un algoritmo eficiente para contar los caracteres de una cadena, teniendo en cuenta también los caracteres Unicode.

Además, es importante tener en cuenta que la longitud de una cadena puede variar según el idioma utilizado. Por ejemplo, en idiomas como el chino o el japonés, un solo carácter puede representar una palabra completa, mientras que en otros idiomas puede requerir varios caracteres para formar una sola palabra.

## Ver también

- [Documentación oficial de Swift sobre la clase String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Uso de Unicode en Swift](https://developer.apple.com/library/archive/documentation/StringsTextFonts/Conceptual/TextAndWebiPhoneOS/WorkingWithText/WorkingWithText.html#//apple_ref/doc/uid/TP40009542-CH4-SW36)