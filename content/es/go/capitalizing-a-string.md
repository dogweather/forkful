---
title:                "Go: Capitalizar una cadena"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

### Por qué capitalizar una cadena de caracteres en Go

Capitalizar una cadena de caracteres es un proceso común en muchos lenguajes de programación. En Go, también es una tarea sencilla y útil, especialmente cuando se trabaja con entradas de usuario o datos externos que pueden no estar formateados adecuadamente. Capitalizar una cadena es simplemente convertir la primera letra de cada palabra en mayúscula, lo que puede facilitar la lectura y comprensión de los datos.

## Cómo capitalizar una cadena en Go

En Go, hay varias formas de capitalizar una cadena, dependiendo de las necesidades específicas de tu programa. Aquí hay algunos ejemplos de cómo hacerlo:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Ejemplo de capitalización utilizando strings.ToUpper:
	cadena := "esto es una prueba"
	fmt.Println(strings.Title(strings.ToLower(cadena))) // Salida: Esto Es Una Prueba
	
	// Ejemplo de capitalización utilizando bytes.ToUpper:
	palabra := []byte("hola mundo")
	fmt.Println(string(bytes.ToUpper(palabra))) // Salida: HOLA MUNDO
}
```

En el primer ejemplo, utilizamos la función `strings.Title` para capitalizar una cadena. Esta función convierte todas las letras iniciales en mayúscula, manteniendo el resto de la cadena en minúsculas. También usamos la función `strings.ToLower` para asegurarnos de que todas las letras se convierten en minúsculas antes de aplicar la función `Title`.

En el segundo ejemplo, utilizamos la función `bytes.ToUpper` para capitalizar una cadena. Esta función convierte todas las letras en mayúscula, incluyendo aquellas que ya están en mayúscula.

Puedes elegir la función que mejor se adapte a tus necesidades, dependiendo de si quieres mantener alguna letra en minúscula o capitalizar todas las letras.

## Profundizando en la capitalización de una cadena en Go

Además de las funciones mencionadas anteriormente, también puedes utilizar la función `unicode.ToTitle` para capitalizar una cadena en Go. Esta función utiliza un algoritmo más complejo que tiene en cuenta las diferentes reglas de capitalización para cada idioma.

También puedes crear tu propia función para capitalizar una cadena, si necesitas un control más específico sobre cómo se capitalizan las letras.

En general, capitalizar una cadena en Go es una tarea sencilla gracias a las diversas funciones y métodos disponibles en el lenguaje.

### Ver también

- [Documentación oficial de Go sobre strings](https://golang.org/pkg/strings/)
- [Ejemplos de capitalización en Go](https://play.golang.org/p/baVgr5NZ4vX)
- [Tutorial sobre cadenas en Go](https://www.tutorialspoint.com/go/go_strings.htm)