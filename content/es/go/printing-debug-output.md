---
title:                "Imprimir salida de depuración"
html_title:           "Go: Imprimir salida de depuración"
simple_title:         "Imprimir salida de depuración"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Imprimir mensajes de depuración, también conocidos como "debug output", es una tarea común en la programación. Esta práctica ayuda a los desarrolladores a entender el comportamiento del código y a encontrar posibles errores y problemas en su sistema. Además, puede ser útil para compartir información importante con otros miembros del equipo.

## ¿Cómo hacerlo?

Para imprimir mensajes de depuración en Go, se puede utilizar la función `fmt.Println()`. Esta función toma cualquier número de argumentos y los imprime en la consola, separados por un espacio. Por ejemplo:

```
fmt.Println("El valor de x es:", x)
fmt.Println("La suma de a y b es:", a+b)
```

Esto imprimirá en la consola:

```
El valor de x es: 10
La suma de a y b es: 25
```

También se pueden utilizar formatos para imprimir valores específicos, como por ejemplo:

```
fmt.Printf("El valor de x es: %d", x)
```
Esto imprimirá solo el valor de `x` en formato decimal.

## Profundizando

Además de la función `fmt.Println()`, se pueden utilizar otras funciones como `fmt.Printf()` y `fmt.Sprintf()` para imprimir mensajes de depuración. También se pueden utilizar estrategias como el logging, que permite guardar los mensajes de depuración en un archivo para su posterior análisis.

Otra práctica común es utilizar constantes para controlar el nivel de mensajes de depuración que se imprimen. Por ejemplo, se pueden definir constantes `DEBUG`, `INFO`, `WARNING` y `ERROR` y luego utilizar una estructura de control para decidir qué mensajes imprimir en base al nivel de depuración establecido.

## Ver también

- [Documentación de la librería fmt en Go](https://golang.org/pkg/fmt/)
- [Ejemplos de depuración en Go](https://yourbasic.org/golang/debugging/)
- [Guía para mejores prácticas en logging en Go](https://medium.com/swlh/better-logging-in-go-9d9e10d2d823)