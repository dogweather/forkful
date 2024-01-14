---
title:    "Swift: Imprimiendo salida de depuración"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir mensajes de depuración en el código es una práctica muy útil para detectar y corregir errores en el proceso de desarrollo de una aplicación. Al imprimir información relevante en la consola, podemos comprender mejor qué está ocurriendo en nuestro programa y, por lo tanto, solucionar problemas más eficientemente.

## Cómo hacerlo

Hay varias formas de imprimir mensajes de depuración en Swift, aquí te presentamos dos opciones:

1. Utilizando la función `print()`: esta es la forma más simple y directa de imprimir un mensaje en la consola. Simplemente escribimos `print("mensaje de depuración")` y el mensaje se mostrará en la consola.

```Swift
print("Este es un mensaje de depuración")
```
Output: Este es un mensaje de depuración

2. Utilizando `debugPrint()`: esta función es específica para imprimir información relacionada al debug. A diferencia de `print()`, `debugPrint()` también imprimirá detalles técnicos sobre el objeto que se está imprimiendo, como su tipo de dato y su ubicación en memoria.

```Swift
let numero = 123

debugPrint(numero)
```
Output: 123 -- actualmente en Int en memory at 0x7fd3da403830>

## Profundizando

Además de imprimir simples mensajes, podemos utilizar la sintaxis de interpolación de cadenas para imprimir valores de variables dentro del mensaje. También podemos agregar una descripción para nuestros mensajes, lo que nos ayudará a identificarlos más fácilmente en la consola. Por ejemplo:

```Swift
let nombre = "Juan"
let edad = 25

print("El nombre del usuario es \(nombre) y tiene \(edad) años.")
```
Output: El nombre del usuario es Juan y tiene 25 años.

Otra opción útil es utilizar la función `assert()` para imprimir un mensaje solo en caso de que se cumpla una determinada condición. Por ejemplo:

```Swift
let calificacion = 7

assert(calificacion >= 6, "El alumno no puede pasar la materia con esa calificación.")
```
Si la calificación es menor a 6, se imprimirá el mensaje de depuración.

## Ver también

- [Documentación oficial de Swift sobre la impresión de mensajes de depuración](https://docs.swift.org/swift-book/LanguageGuide/Printing.html)
- [Tutorial de Ray Wenderlich sobre la impresión de mensajes de depuración en Swift](https://www.raywenderlich.com/5137-how-to-debug-in-swift)
- [Video tutorial de CodeWithChris sobre cómo imprimir mensajes de depuración en Swift](https://www.youtube.com/watch?v=_oR18OX5Ncw)