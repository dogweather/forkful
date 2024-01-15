---
title:                "Capitalizar una cadena"
html_title:           "Swift: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por Qué

Si alguna vez has necesitado convertir una cadena de texto en mayúsculas, ya sea para mostrarla en un título o para validar una entrada de usuario, entonces saber cómo capitalizar una cadena en Swift puede ser de gran ayuda. En este artículo, aprenderás cómo hacerlo de manera sencilla y eficiente.

## Cómo Hacerlo

```Swift
//Definir una cadena de texto
let texto = "hola mundo"

//Usar el método capitalized para convertir la primera letra en mayúscula
let textoCapitalizado = texto.capitalized 

//Mostrar el resultado
print(textoCapitalizado) //Imprimirá "Hola mundo"
```

El método `capitalized` toma la primera letra de cada palabra en la cadena y la convierte en mayúscula, dejando las demás letras como están. Esto es útil si quieres resaltar la primera letra de cada palabra en un título, por ejemplo. También hay otras formas de capitalizar una cadena en Swift, como usando el método `uppercased` para convertir toda la cadena en mayúsculas o `lowercased` para convertirla en minúsculas.

## Profundizando

¿Qué pasa si quieres capitalizar solo una parte de la cadena? Por ejemplo, si tienes un nombre y quieres mostrarlo con la primera letra en mayúscula, pero dejar el resto en minúscula. En ese caso, puedes usar el método `prefix` para seleccionar la parte de la cadena que quieres capitalizar y luego aplicarle el método `capitalized`.

```Swift
let nombreCompleto = "juan perez"
let primerNombre = nombreCompleto.prefix(4) //Obtendrá "juan"

let primerNombreCapitalizado = primerNombre.capitalized //Convertirá "juan" en "Juan"

print(primerNombreCapitalizado) //Imprimirá "Juan"
```

Ten en cuenta que al usar `prefix` necesitas especificar el número de caracteres que quieres seleccionar. También puedes usar `suffix` para seleccionar los últimos caracteres de la cadena.

## Ver También

- [Documentación de Apple sobre `capitalized`](https://developer.apple.com/documentation/swift/string/1780175-capitalized)
- [Todas las formas de manipular cadenas en Swift](https://www.hackingwithswift.com/articles/78/how-to-use-string-interpolation-in-swift)
- [Métodos útiles para trabajar con cadenas en Swift](https://www.swiftbysundell.com/articles/the-different-shades-of-swift-strings/)