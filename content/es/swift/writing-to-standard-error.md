---
title:                "Swift: Escribiendo en el error estándar"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

¿Te has preguntado alguna vez por qué una parte importante de escribir código es asegurarse de que también sea importante escribir al error estándar? Bueno, hoy vamos a sumergirnos en el mundo de escribir al error estándar en Swift y por qué es tan importante en el desarrollo de aplicaciones.

## Cómo escribir al error estándar en Swift

Para escribir al error estándar en Swift, puedes usar la función `print` y pasarle un objeto como argumento. Por ejemplo:

```
let errorMessage = "¡Hubo un error al cargar los datos!"
print(errorMessage)
```

Esta línea de código imprimirá el mensaje de error en la consola. Sin embargo, también puedes utilizar la función `fprint` y pasarle el objeto `stderr` como argumento para escribir directamente en el error estándar. Por ejemplo:

```
let errorMessage = "¡Hubo un error al cargar los datos!"
fprint(stderr, errorMessage)
```

Este código imprimirá el mensaje de error directamente en el error estándar. Es importante tener en cuenta que, al utilizar `print`, el mensaje se envía a la salida estándar (stdout), mientras que al utilizar `fprintf` con `stderr`, el mensaje se envía directamente al error estándar.

## Profundizando en escribir al error estándar

Escribir al error estándar es importante ya que permite a los desarrolladores ver inmediatamente si hay algún problema o error en su código. Los mensajes de error se utilizan para identificar dónde se produjo un error y para ayudar a solucionarlo de manera más eficiente. Además, escribir al error estándar también es útil en situaciones en las que no se puede usar la salida estándar, como en aplicaciones de línea de comandos.

## Ver también

- [Documentación oficial de Swift sobre la función `print`](https://developer.apple.com/documentation/swift/1541053-print)
- [Documentación oficial de Swift sobre la función `fprintf`](https://developer.apple.com/documentation/swift/2839156-fprintf)
- [Tutorial de escritura de mensajes de error en Swift](https://www.raywenderlich.com/6074746-writing-error-messages-in-swift)