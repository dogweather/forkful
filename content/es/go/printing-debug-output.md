---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qué y Por qué?

La impresión de debug output, o salida de depuración, es emitir información útil sobre el estado de tu programa en tiempo de ejecución. Los programadores lo hacen para facilitar la resolución de problemas y entender mejor cómo funciona su código.

## Cómo hacerlo:

Aquí hay un ejemplo básico en Go:

```Go
package main

import (
  "log"
)

func main() {
  log.Println("Esto es un mensaje de depuración")
}
```
Esto imprimirá un mensaje de depuración en la consola:

```
2009/11/10 23:00:00 Esto es un mensaje de depuración
```

## Análisis profundo:

1. **Contexto histórico**: Originalmente, la impresión de salida de depuración se llevaba a cabo mediante el uso de mensajes de consola en el flujo de código. Go, lanzado en 2007, viene con un paquete de log estándar que facilita esta tarea.

2. **Alternativas**: Además de `log.Println`, Go ofrece otras funciones para imprimir mensajes de depuración, como `log.Panicln` y `log.Fatalf`, que también pueden detener la ejecución del programa.

3. **Detalles de implementación**: Cuando usas `log.Println`, no sólo imprimes un mensaje, sino también la fecha y la hora. Esta es una característica útil para la depuración, ya que te permite ver cuándo sucedió algo específico.

## Ver también:

Para aprender más sobre debug output en Go, echa un vistazo a estos recursos:

1. [Paquete de log oficial de Go](https://golang.org/pkg/log/)
2. [Depuración de aplicaciones Go](https://gobyexample.com/logging)
3. [Depuración con el paquete de log](https://www.alexedwards.net/blog/an-overview-of-go-tooling#debugging-and-profiling)