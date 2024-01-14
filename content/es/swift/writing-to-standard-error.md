---
title:    "Swift: Escribiendo en el error estándar"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir en el error estándar en Swift

Cuando estamos escribiendo un programa en Swift, a menudo nos encontramos con errores que pueden ser difíciles de identificar. Es por eso que es importante entender cómo escribir en el error estándar puede ser beneficioso. 

## Cómo hacerlo

Escribir en el error estándar en Swift es muy sencillo. Solo necesitamos usar la función `print()` con el parámetro `to:` y especificar `standardError` como el destino. Por ejemplo:

```Swift
// Código de ejemplo
print("Hola, mundo!", to: &standardError)
```

El resultado de este código sería la impresión de "Hola, mundo!" en la pantalla normal (error estándar). 

## Profundizando

Ahora que sabemos cómo escribir en el error estándar, es importante entender por qué es útil hacerlo. Cuando escribimos en el error estándar, podemos imprimir información adicional acerca de nuestro programa, como mensajes de depuración o detalles de errores, que pueden ser útiles para entender lo que está sucediendo en nuestro código.

También podemos utilizar la función `print(_:separator:terminator:)` para especificar una cadena de separación y terminación, lo que nos permite formatear nuestra salida en el error estándar de una manera más legible. 

Además, escribir en el error estándar puede ser útil para mostrar un mensaje de error personalizado al usuario en caso de que nuestro programa encuentre un error. 

## Ver también

- [Documentación oficial de Swift sobre la función `print()`](https://docs.swift.org/swift-book/LanguageGuide/Functions.html#function-parameters)
- [Artículo sobre depuración de errores en Swift](https://medium.com/flawless-app-stories/debugging-101-breakpoints-1dfb8396b337)