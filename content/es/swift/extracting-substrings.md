---
title:    "Swift: Extrayendo subcadenas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas en Swift es útil?

Extraer subcadenas es una técnica común en programación que nos permite manipular y acceder a partes específicas de una cadena de texto en Swift. Esto puede ser útil para realizar operaciones de búsqueda, reemplazo o formateo de datos en una cadena de texto más larga.

## Cómo hacerlo

Para extraer una subcadena en Swift, utilizamos el método `subString` y especificamos el rango de caracteres que deseamos extraer. Por ejemplo, supongamos que tenemos una cadena de texto "¡Hola, mundo!" y queremos extraer solo la palabra "mundo". Podríamos hacerlo de la siguiente manera:

```Swift
let str = "¡Hola, mundo!"
let subStr = str.substring(with: 7..<12)
print(subStr)

// Output: mundo
```

En este ejemplo, utilizamos el método `substring` y especificamos el rango del carácter 7 al 11 (ya que los índices comienzan en 0). Esto nos devuelve una nueva cadena "mundo" que podemos utilizar para cualquier propósito que necesitemos.

Además de especificar un rango de caracteres, también podemos utilizar el método `substring` para extraer una subcadena desde un índice específico hasta el final de la cadena. Por ejemplo, si tenemos la misma cadena "¡Hola, mundo!" y solo queremos extraer la palabra "mundo" sin especificar su posición inicial, podríamos hacerlo así:

```Swift
let str = "¡Hola, mundo!"
let subStr = str.substring(from: 7)
print(subStr)

// Output: mundo!
```

En este caso, utilizamos el método `substring` con el parámetro `from` y especificamos que queremos extraer la subcadena desde el índice 7 hasta el final de la cadena.

Otra forma de extraer subcadenas en Swift es utilizando la notación de rangos. Por ejemplo, si queremos extraer los primeros 5 caracteres de la cadena "¡Hola, mundo!", podríamos hacer lo siguiente:

```Swift
let str = "¡Hola, mundo!"
let subStr = str[..<5]
print(subStr)

// Output: ¡Hola
```

En este caso, utilizamos la notación de rango de la forma `[.. <n]` para especificar que queremos extraer los primeros 5 caracteres de la cadena.

## Análisis en profundidad

Además de los métodos mencionados anteriormente, Swift también nos permite utilizar la función `index` para acceder a un carácter específico de una cadena. Por ejemplo, si queremos acceder al primer carácter de la cadena "¡Hola, mundo!", podríamos hacerlo de la siguiente manera:

```Swift
let str = "¡Hola, mundo!"
let firstChar = str[str.startIndex]
print(firstChar)

// Output: ¡
```

Aquí, utilizamos la función `index` con el parámetro `startIndex` para acceder al primer carácter de la cadena. También podemos utilizar la función `index` junto con la función `count` para acceder al último carácter de la cadena, como se muestra a continuación:

```Swift
let str = "¡Hola, mundo!"
let lastChar = str[str.index(str.endIndex, offsetBy: -1)]
print(lastChar)

// Output: !
```

En este caso, utilizamos la función `index` y le pasamos como parámetro `endIndex` junto con un desplazamiento de -1 para acceder al último carácter de la cadena.

## Ver también

- [Documentación oficial de Swift sobre la extracción de subcadenas](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
- [Tutorial de Hacking with Swift sobre la extracción de subcadenas](https://www.hackingwithswift.com/read/0/15/substrings-with-nsrange)
- [Ejemplos de código de Swift en GitHub para la extracción de subcadenas](https://github.com/search?l=Swift&q=substring&type=Code)