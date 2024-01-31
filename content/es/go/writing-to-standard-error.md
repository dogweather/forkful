---
title:                "Escribiendo en el error estándar"
date:                  2024-01-19
simple_title:         "Escribiendo en el error estándar"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir en el error estándar es enviar mensajes de error a un flujo específico, separado de la salida estándar. Los programadores lo hacen para diferenciar la salida normal de la información de diagnóstico y errores, ayudando en la depuración y el registro de eventos.

## Cómo se Hace:

Escribir en el error estándar en Go se hace usando el paquete `os` y su variable `Stderr`.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	errMsg := "Hubo un error crítico en el proceso."

	// Escribir en el error estándar
	_, err := fmt.Fprintln(os.Stderr, errMsg)
	if err != nil {
		panic("No se pudo escribir el mensaje en el error estándar.")
	}
}
```

Salida esperada en el error estándar:

```
Hubo un error crítico en el proceso.
```

## Profundizando:

Históricamente, la separación de la salida estándar y el error estándar proviene de Unix, donde las herramientas de línea de comando utilizan tres flujos principales: entrada estándar (stdin), salida estándar (stdout) y error estándar (stderr).

Alternativas a `fmt.Fprintln(os.Stderr, errMsg)` incluyen el uso de `log` o `os.Stderr.WriteString(errMsg+"\n")`. La implementación básica de estas funciones eventualmente hace llamadas al sistema para escribir en el file descriptor asociado con stderr, que generalmente es el número 2.

## Ver También:

Para aprender más sobre los flujos de salida en Go y la gestión de errores, visita los siguientes enlaces:

- Documentación de Go para el paquete os: https://pkg.go.dev/os
- Blog de Go sobre manejo de errores: https://blog.golang.org/error-handling-and-go
- Ejemplos de como utilizar el paquete `log` para errores: https://pkg.go.dev/log

Recuerda también explorar las funciones como `log.Fatalf` o `log.Panicf` que también escriben en el error estándar y manejan los errores de forma ligeramente diferente, dependiendo de tus necesidades específicas.
