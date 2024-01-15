---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Swift: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué
Eliminar caracteres que coinciden con un patrón es una tarea común en la programación y puede ser útil para limpiar entradas de datos o realizar búsquedas más específicas en una cadena de caracteres.

## Cómo hacerlo
La función `replacingOccurrences(of:with:)` de Swift permite reemplazar una cadena de caracteres con otra en función de un patrón. Por ejemplo, si queremos eliminar todas las vocales de una palabra, podemos utilizar la siguiente línea de código:

```Swift
let palabra = "programación"
let palabraSinVocales = palabra.replacingOccurrences(of: "[aeiou]", with: "", options: .regularExpression)
```
La variable `palabra` contiene la cadena original y en `palabraSinVocales` se almacenará el resultado, en este caso "prgrmcón". Al utilizar `options: .regularExpression`, le estamos indicando a Swift que queremos utilizar expresiones regulares para buscar el patrón que le hemos especificado entre corchetes (`[aeiou]`, en este caso).

Otra opción es utilizar la función `filter` para crear un nuevo arreglo con los caracteres que no cumplan con nuestro patrón. Por ejemplo, si queremos eliminar todas las letras minúsculas de una cadena, podemos hacer lo siguiente:

```Swift
let cadena = "Código123"
let cadenaSoloNumeros = cadena.filter { !"abcdefghijklmnopqrstuvwxyz".contains($0) }
```

En este caso, la variable `cadenaSoloNumeros` contendrá "123".

## Profundizando
Como mencionamos antes, utilizar expresiones regulares es una forma poderosa y versátil de buscar patrones en una cadena de caracteres. En el ejemplo anterior, `[aeiou]` representa una clase de caracteres, lo que significa que cualquier vocal (minúscula o mayúscula) será reemplazada. Sin embargo, podemos utilizar otros caracteres especiales para realizar búsquedas más complejas, como por ejemplo:

- `.`: coincide con cualquier caracter
- `\d`: coincide con un dígito
- `\w`: coincide con un caracter alfanumérico
- `\s`: coincide con un espacio en blanco
- `^`: coincide con el inicio de la cadena
- `$`: coincide con el final de la cadena
- `*`: coincide con 0 o más repeticiones de un patrón
- `+`: coincide con 1 o más repeticiones de un patrón
- `?`: coincide con 0 o 1 repetición de un patrón
- `{n}`: coincide con exactamente `n` repeticiones de un patrón
- `{n,}`: coincide con al menos `n` repeticiones de un patrón
- `{n,m}`: coincide con entre `n` y `m` repeticiones de un patrón

Puedes encontrar más información sobre expresiones regulares en la [documentación oficial de Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID291).

## Ver también
- [Cómo utilizar expresiones regulares en Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)
- [Documentación oficial de Swift sobre strings y caracteres](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)