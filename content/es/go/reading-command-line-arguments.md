---
title:                "Lectura de argumentos de línea de comandos"
date:                  2024-01-20T17:56:14.055637-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Leer argumentos de la línea de comandos permite que tu programa en Go interactúe con el usuario a través de la terminal. Los programadores usan esta técnica para personalizar la ejecución del programa sin cambiar el código.

## Cómo:

Aquí está lo básico. Importa el paquete `os` y usa `os.Args` para acceder a los argumentos de la línea de comandos:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// os.Args[0] es el nombre del programa
	// Los argumentos empiezan en os.Args[1]
	arguments := os.Args[1:]
	for _, arg := range arguments {
		fmt.Println(arg)
	}
}
```

Si corres tu programa (`go run your_program.go arg1 arg2`) con argumentos, recibirás:

```
arg1
arg2
```

## Inmersión Profunda:

En Go, `os.Args` es un slice que contiene todas las entradas pasadas al programa. El primer elemento siempre es el nombre del ejecutable. Fue diseñado inspirándose en C, donde `argv` y `argc` hacen un trabajo similar.

Para casos más avanzados o cuando necesitas más control, usa el paquete `flag`. Este te permite definir y parsear argumentos de línea de comandos con mayor facilidad. Permite especificar tipos, valores predeterminados y mensajes de ayuda automáticamente.

Dato curioso: antes de `flag`, las opciones de línea de comandos en Go fueron manejadas de manera ad hoc. Con `flag`, Go ofreció una estrategia consistente y potente.

Detalles de implementación: cuando se trabaja con `os.Args`, recuerda que todos los argumentos son strings. Si necesitas convertirlos a otros tipos, tendrás que hacerlo manualmente.

## Ver También:

- Documentación oficial del paquete `os`: https://golang.org/pkg/os/
- Tutorial del paquete `flag`: https://golang.org/pkg/flag/
- Blog post sobre cómo usar los argumentos de la línea de comandos en Go: https://blog.golang.org/command-line-arguments
