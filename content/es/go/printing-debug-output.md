---
title:                "Go: Impresión de salida de depuración"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

Imprimir mensajes de depuración es una parte importante del proceso de programación en Go. Al mostrar o registrar información en la consola, podemos entender mejor cómo nuestro código está funcionando y encontrar posibles errores.

## Cómo hacerlo

Para imprimir mensajes de depuración en Go, podemos utilizar la función `fmt.Println()`. Por ejemplo:

```Go
package main

import "fmt"

func main() {
  nombre := "Juan"
  edad := 25
  fmt.Println("El nombre del usuario es", nombre, "y tiene", edad, "años.")
}
```

Este código producirá la siguiente salida en la consola:

`El nombre del usuario es Juan y tiene 25 años.`

Podemos también imprimir valores de variables de diferentes tipos de datos, como enteros, flotantes y booleanos utilizando la función `fmt.Printf()`. Por ejemplo:

```Go
package main

import "fmt"

func main() {
  altura := 1.75
  esCasado := true
  fmt.Printf("La altura del usuario es %.2f y su estado civil es %t", altura, esCasado)
}
```

La salida de este código será:

`La altura del usuario es 1.75 y su estado civil es true`

En estos ejemplos, estamos utilizando el operador `%` para indicar dónde queremos imprimir cada valor. El `.` seguido de un número en la cadena de formato, indica cuántos decimales queremos mostrar en el caso de números. También podemos utilizar `%s` para imprimir cadenas y `%v` para imprimir el valor predeterminado de una variable.

## Profundizando en la impresión de mensajes de depuración

Cuando estamos depurando un programa, es importante tener en cuenta que imprimir demasiados mensajes puede hacer que la consola se vuelva confusa y difícil de leer. Por lo tanto, debemos ser selectivos en qué información imprimimos y dónde lo hacemos.

Una alternativa a la impresión de mensajes de depuración en la consola es utilizar un paquete de registro, como `log` o `logrus`, que nos permite guardar mensajes en un archivo en lugar de solo imprimirlos en la consola. También podemos utilizar niveles de registro para mostrar diferentes tipos de mensajes, como mensajes de información, advertencias o errores.

## Ver también

- [Documentación oficial de Go sobre la función fmt](https://pkg.go.dev/fmt)
- [Ejemplo de uso de paquetes de registro en Go](https://www.golangprograms.com/go-language/advance-programming/go-logging-package-use-in-logrus-example.html)