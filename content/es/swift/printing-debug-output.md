---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por qué?

La impresión de Debug Output es una técnica que los programadores utilizan para rastrear y entender el flujo de un programa. Nos permite detectar y solucionar errores efectivamente.

## ¿Cómo hacerlo?

Puedes imprimir información a consola en Swift con la función `print()`. Observa el siguiente ejemplo:

```Swift
var name = "Juan"
print("Hola, \(name)!")
```

Esta parte del código imprimirá el texto `Hola, Juan!`.

También puedes usar `debugPrint()` para una salida más detallada cuando trabajas con Colecciones.

```Swift
let array = ["Manzana", "Banano", "Uva"]
debugPrint(array)
```

La salida será:

```Swift
["Manzana", "Banano", "Uva"]
```

## Profundizando

La función `print()` ha existido desde el origen de los lenguajes de programación para ayudar en tareas de depuración. Sin embargo, Swift también ofrece `dump()`, que proporciona información adicional, como los índices y los subelementos de las Colecciones.

Aquí una muestra usando `dump()`:

```Swift
dump(array)
```

En este caso la salida mostrará algo más detallado:

```Swift
- ["Manzana", "Banano", "Uva"]
  - "Manzana"
  - "Banano"
  - "Uva"
```

## Ver También

Para más detalles acerca de salida Debug y funciones relacionadas en Swift, puedes visitar la documentación oficial de Apple para [print()](https://developer.apple.com/documentation/swift/1541053-print), [debugPrint()](https://developer.apple.com/documentation/swift/1542962-debugprint), y [dump()](https://developer.apple.com/documentation/swift/1538991-dump).