---
title:                "Swift: Convirtiendo una cadena a minúsculas"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertingir una cadena de texto a minúsculas es una tarea común en el desarrollo de aplicaciones. Al hacerlo, aseguramos que el texto sea uniforme y fácil de comparar con otras cadenas.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Swift, podemos utilizar el método `lowercased()` en una instancia de `String`. Veamos un ejemplo:

```Swift
let nombre = "Juan"
let nombreEnMinusculas = nombre.lowercased()
print(nombreEnMinusculas)
```

El código anterior imprimirá "juan" en la consola. También podemos asignar directamente el resultado de `lowercased()` a la variable `nombre`:

```Swift
nombre = nombre.lowercased()
print(nombre)
```

En este caso, la variable `nombre` será actualizada con el nuevo valor "juan".

## Profundizando

Cuando utilizamos el método `lowercased()`, podemos encontrarnos con algunos casos particulares que debemos tener en cuenta. Por ejemplo, en algunos idiomas, hay letras que tienen una forma en mayúscula y otra en minúscula, como la "I" y la "ı" en turco. En estos casos, el método `lowercased()` toma en cuenta estas diferencias y devuelve el resultado esperado.

Sin embargo, hay casos en los que el resultado no será el esperado, como en el idioma alemán, donde la letra "ß" se convierte a "ss" en minúsculas en lugar de "ß". Por lo tanto, es importante tener en cuenta estas particularidades al utilizar el método `lowercased()`.

## Ver también

- [Documentación oficial de Apple sobre `lowercased()`](https://developer.apple.com/documentation/swift/string/2427944-lowercased)
- [Convertir una cadena de texto a mayúsculas en Swift](https://www.example.com/convertir-una-cadena-de-texto-a-may%C3%BAsculas-en-swift)
- [Cómo comparar cadenas de texto en Swift](https://www.example.com/c%C3%B3mo-comparar-cadenas-de-texto-en-swift)