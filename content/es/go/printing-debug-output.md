---
title:                "Impresión de salida de depuración"
html_title:           "Go: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imprimir salida de depuración es cuando los programadores muestran información adicional durante la ejecución de un programa para ayudar a identificar y solucionar problemas. Esto se hace generalmente agregando líneas de código que imprimen variables o mensajes específicos en la terminal o en un archivo de registro.

Los programadores utilizan la impresión de salida de depuración para entender el comportamiento de su código, identificar errores y mejorar su programación en general.

## Cómo:

Siguiendo el lenguaje de programación Go, para imprimir una variable llamada "nombre" y un mensaje de error, podríamos escribir lo siguiente:

```
Go fmt.Println(nombre)
Go fmt.Println("Error: No se pudo encontrar el archivo.")
```

La salida se mostrará en la terminal de la siguiente manera:

```
Juan
Error: No se pudo encontrar el archivo.
```

## Profundizando:

La impresión de salida de depuración ha sido una técnica utilizada durante mucho tiempo por los programadores para facilitar el proceso de depuración. Antes de los lenguajes de programación modernos que ofrecen herramientas de depuración avanzadas, imprimir variables y mensajes era la forma más común de entender el flujo de un programa.

Hay alternativas a la impresión de salida de depuración, como herramientas de depuración incorporadas en los IDE o el uso de pruebas unitarias. Sin embargo, imprimir salida de depuración sigue siendo una técnica muy útil y sencilla de implementar en el proceso de desarrollo de software.

En Go, la función "fmt.Println()" es la forma más común de imprimir salida de depuración. Esta función es parte del paquete "fmt" que ofrece una variedad de funciones para formatear y mostrar datos.

## Ver también:

- Documentación oficial de la función "fmt.Println()": https://golang.org/pkg/fmt/#Println
- Tutorial de depuración en Go: https://golangbot.com/debugging-go-programs/