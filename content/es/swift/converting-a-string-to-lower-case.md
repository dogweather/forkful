---
title:    "Swift: Convirtiendo una cadena a minúsculas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena a minúsculas puede ser útil en muchas situaciones de programación, como la comparación de cadenas, la normalización de datos y la corrección de entradas de usuario. También puede ser necesario para cumplir con ciertos requerimientos de formato o para facilitar la manipulación de cadenas.

## Cómo hacerlo

En Swift, puedes convertir una cadena a minúsculas utilizando el método `lowercased()` en una instancia de `String`:

```Swift
let cadena = "Hola Mundo"
let cadenaMin = cadena.lowercased()
print(cadenaMin)
// Salida: hola mundo
```

También puedes utilizar el operador `=` para asignar el valor de la cadena original a una nueva variable modificada:

```Swift
var cadena = "SWIFT"
var cadenaMin = cadena.lowercased()
// La variable 'cadena' permanece igual, pero 'cadenaMin' es ahora igual a "swift"
```

## Profundizando

Es importante tener en cuenta que la conversión a minúsculas también afecta a los caracteres acentuados o con tilde en el idioma español. Por ejemplo, la letra "É" se convertirá a "é" y la letra "Ñ" se convertirá a "ñ".

Además, si la cadena original contiene caracteres no alfabéticos o emojis, estos no se verán afectados por el método `lowercased()` y permanecerán igual. Por ejemplo, la cadena "Hola 😃" se convertirá a "hola 😃" después de utilizar el método.

Es importante tener en cuenta estos detalles al convertir cadenas a minúsculas en tu código y asegurarte de que la salida sea la esperada.

## Ver también

- Documentación oficial de Apple sobre `lowercased()`: https://developer.apple.com/documentation/foundation/nsstring/1411818-lowercased
- Tutorial de Swift en español: https://www.ios-blog.es/swift-comienza-a-programar-en-este-lenguaje-mas-que-intuitivo/