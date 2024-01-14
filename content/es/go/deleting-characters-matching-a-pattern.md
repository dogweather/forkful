---
title:    "Go: Borrando caracteres que coincidan con un patrón"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Muchas veces en la programación, nos encontramos con la necesidad de eliminar ciertos caracteres que coinciden con un patrón específico en un texto. Ya sea para limpiar datos, validar entradas de usuario o realizar búsquedas en un archivo, esta tarea puede ser crucial para el buen funcionamiento de nuestro programa.

## Cómo hacerlo

En Go, podemos utilizar la función `Regexp.ReplaceAllString()` del paquete `regexp` para eliminar todos los caracteres que coinciden con un patrón en una cadena. Primero, debemos compilar una expresión regular con el patrón que deseamos buscar, utilizando la función `regexp.Compile()` y luego utilizar la función `ReplaceAllString()` pasando la cadena original y una cadena vacía como argumentos.

En el siguiente ejemplo, eliminaremos todas las vocales de una palabra. Dentro de un bloque de código `Go`, se vería así:

```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	original := "hola mundo"
	patron, _ := regexp.Compile("[aeiou]")
	limpiado := patron.ReplaceAllString(original, "")
	fmt.Println(limpiado) //h l mnd
}
```

También podemos utilizar la función `Regexp.ReplaceAll()` para eliminar los caracteres que coinciden con un patrón en una cadena. En este caso, en lugar de pasar una cadena vacía como segundo argumento, podemos utilizar una función que reciba el subparte coincidente y devuelva la cadena vacía. Este enfoque puede ser útil si queremos realizar algún tipo de transformación en los caracteres coincidentes antes de eliminarlos.

## Profundizando

El paquete `regexp` también nos ofrece diferentes opciones para trabajar con expresiones regulares. Por ejemplo, podemos utilizar `Regexp.FindAllString()` para encontrar todas las subpartes en una cadena que coinciden con un patrón. Y si necesitamos unir varios patrones en una sola expresión regular, podemos utilizar el operador `|` para separarlos.

Además, también es posible utilizar expresiones regulares para validar entradas de usuario, como contraseñas o direcciones de correo electrónico, y garantizar que cumplan con ciertos criterios. En general, utilizar expresiones regulares puede ayudarnos a agilizar nuestro código y evitar la necesidad de utilizar largas y complejas cadenas de condicionales.

## Ver también
- Documentación oficial del paquete `regexp`: https://golang.org/pkg/regexp/
- Tutorial interactivo sobre expresiones regulares en Go: https://regex-golang.appspot.com/
- Ejemplos prácticos de cómo utilizar expresiones regulares en Go: https://gobyexample.com/regular-expressions