---
title:    "Swift: Extracción de subcadenas"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por Qué

Extraer subcadenas es una habilidad esencial para manipular y trabajar con cadenas de texto en Swift. Al hacerlo, puedes obtener partes específicas de una cadena y utilizarlas de forma individual para realizar diversas operaciones y tareas.

## Cómo Hacerlo

Para extraer subcadenas en Swift, existen varias opciones y métodos disponibles. Aquí hay un ejemplo usando la función `substring`:

```Swift
let str = "¡Hola mundo!"
let startIndex = str.index(str.startIndex, offsetBy: 6)
let endIndex = str.index(str.endIndex, offsetBy: -1)
let newStr = str.substring(with: startIndex..<endIndex)
print(newStr)
```

En este ejemplo, establecemos una cadena original "¡Hola mundo!" y luego utilizamos la función `substring` para extraer una subcadena que comienza en el índice 6 (que corresponde a la primera letra "m") y termina en el índice -1 (que corresponde a la última letra "!"). Esto nos da una subcadena "mundo" que imprimimos en la consola.

## Profundizando

Además de la función `substring`, también puedes utilizar otros métodos como `prefix`, `suffix` y `dropFirst` para extraer subcadenas en Swift. Cada uno tiene un propósito específico, por lo que puede ser útil experimentar con ellos y ver cuál se adapta mejor a tu código.

También es importante tener en cuenta que al extraer subcadenas, debes prestar atención a los índices y asegurarte de que no te salgas del rango de la cadena original para evitar errores.

## Ver También

- [Documentación oficial de Swift sobre manipulación de cadenas](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#)
- [Tutorial sobre extracción de subcadenas en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Más sobre manipulación de cadenas en Swift](https://www.raywenderlich.com/137516/swift-tutorial-part-2-simple-ios-app)