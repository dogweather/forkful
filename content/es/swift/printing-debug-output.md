---
title:                "Imprimiendo resultados de depuración"
html_title:           "Swift: Imprimiendo resultados de depuración"
simple_title:         "Imprimiendo resultados de depuración"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has encontrado en una situación en la que tu código no funciona como esperas y no tienes idea de por qué? ¡Ahí es donde la impresión de salida de depuración entra en juego! Imprimir resultados y datos de depuración en la consola puede ser una forma muy útil de entender lo que está sucediendo en tu código y encontrar errores.

## Cómo hacerlo

Para imprimir en la consola en Swift, puedes utilizar la función `print()`. Esta función toma un argumento (el dato que quieras imprimir) y lo muestra en la consola. Aquí hay un ejemplo sencillo:

```Swift
let mensaje = "¡Hola mundo!"
print(mensaje)
```

Esto imprimirá "¡Hola mundo!" en la consola. Sin embargo, también puedes imprimir más de un dato a la vez separándolos con comas dentro de la función `print()`:

```Swift 
let numero = 27
let decimal = 3.14
print("El número es", numero, "y el valor decimal es", decimal)
```

Esto imprimirá "El número es 27 y el valor decimal es 3.14" en la consola.

Si quieres imprimir el valor de una variable o constante solo tienes que incluirla dentro de la función `print()`. Y si quieres imprimir el valor de una variable o constante junto con un mensaje, puedes utilizar la interpolación de cadenas de Swift utilizando el símbolo `\()`:

```Swift
let edad = 21
print("Mi edad es \(edad) años")
```

Esto imprimirá "Mi edad es 21 años" en la consola.

## Profundizando

Además de imprimir resultados y datos de depuración, también puedes utilizar la función `print()` para comprobar si ciertas partes de tu código se están ejecutando correctamente. Por ejemplo, si tienes un bucle `for` y quieres comprobar el valor de una variable en cada iteración, puedes imprimirlo dentro del bucle:

```Swift
for i in 1...5 {
    print(i)
}
```

Esto imprimirá los números del 1 al 5 en la consola en cada iteración del bucle.

También puedes utilizar la función `print()` para imprimir mensajes de error en la consola en lugar de simplemente detener la ejecución de tu código. Esto puede ser útil para encontrar rápidamente dónde se encuentra un error en tu código.

Recuerda que al finalizar la depuración de tu código, es importante eliminar todas las impresiones de salida que hayas utilizado. Esto garantizará que tu código sea más eficiente y no muestre información innecesaria en la consola.

## Ver también

- [Documentación de Swift sobre la función `print()`](https://developer.apple.com/documentation/swift/1541053-print)
- [Tutorial de depuración en Swift](https://www.hackingwithswift.com/quick-start/debugging/how-to-use-swift-to-debug-your-code)
- [Guía de depuración de código en Xcode](https://developer.apple.com/library/archive/documentation/DeveloperTools/Conceptual/debugging_with_xcode/chapters/debugging_tools.html#//apple_ref/doc/uid/TP40015022-CH8-SW1)