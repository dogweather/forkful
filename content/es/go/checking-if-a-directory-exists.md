---
title:                "Verificando si un directorio existe"
html_title:           "Go: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Verificar si un directorio existe es una operación común utilizada en la programación para confirmar la existencia de un directorio en una ruta específica. Esto es útil para evitar errores al intentar acceder a un directorio que no existe.

## Cómo hacerlo:

Aquí hay un ejemplo simple usando la función `Stat` del paquete `os` en Go:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
     // cambiar a su directorio
	dirPath := "./test-dir"

	_, err := os.Stat(dirPath)

	if os.IsNotExist(err) {
		fmt.Printf("El directorio %v no existe. \n", dirPath)
	} else {
		fmt.Printf("El directorio %v existe. \n", dirPath)
	}
}
```

Esto imprimirá "El directorio ./test-dir existe." si el directorio existe o "El directorio ./test-dir no existe." si no existe.

## Deep Dive:
Verificar si un directorio existe en Go ha sido una necesidad desde los primeros días del lenguaje. `os.Stat` y `os.IsNotExist` son las dos funciones más comunes para realizar este control.

Históricamente es común también en otros lenguajes de programación, como Python y Java. Go lo hace de una manera más simplificada y directa.

Una alternativa a `os.Stat` es usar `os.Open` y después `File.Stat` de esta manera:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
     // cambiar a su directorio
	dirPath := "./test-dir"

	_, err := os.Open(dirPath)

	if os.IsNotExist(err) {
		fmt.Printf("El directorio %v no existe. \n", dirPath)
	} else {
		fmt.Printf("El directorio %v existe. \n", dirPath)
	}
}
```

`os.Stat` retorna una estructura `FileInfo` que describe el archivo/directorio nombrado y un error, archivo/directorio es el que se pasó por parámetro. `os.IsNotExist` es una función que recibe un error como parámetro y nos dice si este corresponde a un archivo/directorio que no existe.

## Ver también:

Para más información y recursos en línea, echa un vistazo a los siguientes enlaces:

1. Documentación oficial de Go: paquete os: [os package](https://golang.org/pkg/os/)
2. Descripción detallada de la función os.Stat y los errores del paquete os: [os.Stat function](https://golang.org/pkg/os/#Stat) y [os package errors](https://golang.org/pkg/os/#IsNotExist)
3. Foro oficial de Go: [forum Golang](https://forum.golangbridge.org/)