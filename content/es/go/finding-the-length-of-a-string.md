---
title:                "Encontrar la longitud de un string"
html_title:           "Go: Encontrar la longitud de un string"
simple_title:         "Encontrar la longitud de un string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué encontrar la longitud de una cadena?

En la programación, a menudo necesitamos trabajar con cadenas de texto y conocer su longitud puede ser crucial para realizar ciertas operaciones. La función para encontrar la longitud de una cadena en Go es simple y útil en muchas situaciones.

## Cómo hacerlo

El paquete `strings` de Go tiene una función llamada `len` que nos permite encontrar la longitud de una cadena. Veamos un ejemplo de cómo usarla:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "¡Hola Mundo!"
	length := len(str)

	fmt.Println("La longitud de la cadena es:", length) // salida: 12
}
```
En este ejemplo, primero importamos el paquete `strings` que contiene la función `len`. Luego, declaramos una variable `str` con la cadena de texto que queremos encontrar su longitud. Finalmente, usamos la función `len` y le pasamos la variable `str` como argumento para obtener la longitud de la cadena.

También se pueden encontrar la longitud de otras estructuras de datos, como un array o un slice de bytes. Aquí hay un ejemplo:

```Go
package main

import "fmt"

func main() {
	arr := [4]string{"Hola", "Mundo", "en", "Go"}
	length := len(arr)

	fmt.Println("La longitud del array es:", length) // salida: 4

	slice := []byte("¡Hola Mundo!")
	length := len(slice)

	fmt.Println("La longitud del slice es:", length) // salida: 12
}
```
Comenzamos declarando un array de cadenas y una variable `length` para almacenar su longitud. Usamos la función `len` con el array y obtenemos la longitud correcta. Luego, declaramos un slice de bytes a partir de la cadena "¡Hola Mundo!" y nuevamente utilizamos la función `len` para encontrar su longitud.

## Profundizando

La función `len` en Go es muy eficiente y devuelve el número de bytes de una cadena, no el número de caracteres. Esto es importante tener en cuenta ya que algunos caracteres pueden ocupar más de un byte en memoria. Por ejemplo, el carácter "á" ocupa 2 bytes.

Además, la función `len` solo puede usarse en valores que implementen la interfaz `Length` o `Readable`, lo que significa que solo se puede usar en tipos que tengan una propiedad de longitud definida. Esto incluye cadenas, arrays, slices, maps y canales.

## Ver también

- Documentación oficial sobre la función `len` en Go: https://golang.org/pkg/builtin/#len
- Tutorial sobre cadenas en Go: https://www.golang-book.com/books/intro/8#section1