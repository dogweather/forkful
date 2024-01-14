---
title:    "Go: Escribiendo en el error estándar"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error (stderr) puede ser útil cuando se están realizando pruebas o depurando un programa en Go. Al imprimir los mensajes de error en lugar de simplemente mostrarlos en la consola, se puede tener un mejor control y registro de los errores que surjan.

## Cómo hacerlo

Para escribir a la salida de error en Go, se puede utilizar la función `fmt.Fprintln` junto con `os.Stderr`. En el siguiente ejemplo, se muestra cómo imprimir un mensaje de error en stderr y cómo obtener la ruta del directorio actual:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Imprimir mensaje de error en stderr
	fmt.Fprintln(os.Stderr, "Ocurrió un error")

	// Obtener ruta del directorio actual
	fmt.Fprintln(os.Stderr, "Directorio actual:", os.Getwd())
}
```

El output de este programa sería:

```
Ocurrió un error
Directorio actual: /home/usuario/go
```

## Profundizando

Si se está trabajando con un programa en Go que necesite escribir a la salida de error de forma regular, es importante tener en cuenta que esto puede afectar el rendimiento del programa. Por lo tanto, es importante utilizar esta técnica solo cuando sea necesario y asegurarse de gestionar correctamente los errores.

Además, es posible utilizar paquetes como `log` para imprimir mensajes de error en stderr con más facilidad y proporcionar opciones adicionales, como imprimir la fecha y hora del error.

## Ver también

- [Documentación de fmt](https://golang.org/pkg/fmt/)
- [Documentación de os](https://golang.org/pkg/os/)
- [Documentación de log](https://golang.org/pkg/log/)