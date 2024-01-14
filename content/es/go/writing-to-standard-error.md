---
title:                "Go: Escribiendo en el error estándar"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en el error estándar en Go?

Escribir en el error estándar es una práctica común en la programación en Go. Al enviar mensajes de error a esta salida, podemos monitorear y depurar nuestro código de manera más eficiente.

## Cómo hacerlo

En Go, podemos escribir en el error estándar utilizando la función `fmt.Fprintf()`. Aquí hay un ejemplo de cómo hacerlo:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {

	// Se declara la variable para guardar el mensaje de error
	errorMsg := "¡Algo salió mal!"

	// Se escribe en el error estándar
	fmt.Fprintf(os.Stderr, "%s\n", errorMsg)
}
```

Este código imprimirá el mensaje de error en la salida estándar de errores, en este caso, la consola. La sintaxis de `fmt.Fprintf()` es similar a la de `fmt.Printf()` pero en este caso utilizamos `os.Stderr` como el primer argumento para especificar que escribir en la salida de errores.

## Profundizando en la escritura en el error estándar

Además de simplemente enviar mensajes de error, también podemos formatearlos de manera específica y añadir información adicional. Por ejemplo, si queremos incluir el nombre del archivo y la línea donde ocurrió un error, podemos hacer lo siguiente:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {

	// Se declara el archivo y la línea donde ocurrió el error
	fileName := "miArchivo.txt"
	lineNum := 10

	// Se escribe en el error estándar con formato
	fmt.Fprintf(os.Stderr, "¡Error en %s en la línea %d!\n", fileName, lineNum)
}
```

Esto imprimirá el siguiente mensaje de error en la consola: `¡Error en miArchivo.txt en la línea 10!`. Esto puede ayudar a los desarrolladores a identificar y resolver problemas en su código de manera más eficiente.

## Véase también

- Documentación oficial de Go sobre `fmt.Fprintf()`: https://golang.org/pkg/fmt/#Fprintf
- Cómo manejar errores en Go: https://blog.golang.org/error-handling-and-go
- Tutorial de Go: https://tour.golang.org/welcome/1