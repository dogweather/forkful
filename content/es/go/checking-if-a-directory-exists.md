---
title:    "Go: Comprobando si existe un directorio"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué comprobar si un directorio existe en Go

La comprobación de la existencia de un directorio es una parte importante de la programación en Go. Esta función ayuda a garantizar que el código se ejecute correctamente y se eviten errores al tratar de acceder a un directorio que no existe. Además, puede ser útil para verificar si un directorio previamente creado se ha eliminado o no.

## Cómo hacerlo

Para comprobar si un directorio existe en Go, utilizaremos la función `os.Stat()` junto con un condicional `if`. Esta función toma como argumento la ruta del directorio que queremos comprobar y devuelve un objeto `os.FileInfo` si el directorio existe, o un error si no existe.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	// Ruta del directorio a comprobar
	dir := "/ruta/del/directorio"

	// Comprobamos si el directorio existe
	if _, err := os.Stat(dir); err == nil {
		fmt.Printf("El directorio %s existe\n", dir)
	} else {
		fmt.Printf("El directorio %s no existe\n", dir)
	}
}
```

Si el directorio existe, se imprimirá "El directorio {ruta del directorio} existe". De lo contrario, se imprimirá "El directorio {ruta del directorio} no existe".

## Profundizando

Además de la función `os.Stat()`, existen otras formas de comprobar la existencia de un directorio en Go. Por ejemplo, la función `os.IsExist()` puede usarse para verificar si se produce un error debido a que el directorio ya existe. También podemos utilizar la función `os.IsNotExist()` para verificar si se produce un error debido a que el directorio no existe.

Además, si necesitamos trabajar con directorios de forma más compleja, existen paquetes de terceros, como el paquete `path/filepath` que proporciona funciones para buscar en directorios y realizar operaciones de comparación.

## Ver también

- Documentación oficial de Go sobre la función `os.Stat()`: https://golang.org/pkg/os/#Stat
- Ejemplos adicionales de códigos para comprobar la existencia de directorios en Go: https://www.golangprograms.com/how-do-i-check-if-a-file-exists-in-go-lang.html