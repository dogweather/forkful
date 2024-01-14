---
title:    "Go: Capitalizando una cadena"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué capitalizar una cadena en Go
En la programación, es común encontrarse con casos en los que necesitamos modificar una cadena de texto para que comience con mayúscula. Esto puede ser por razones estéticas o para cumplir con ciertas reglas de formato. En este artículo, exploraremos cómo capitalizar una cadena en el lenguaje de programación Go.

## Cómo hacerlo
En Go, podemos capitalizar una cadena utilizando la función `strings.Title()`. Esta función toma una cadena como argumento y devuelve una nueva cadena con la primera letra de cada palabra en mayúscula.

Veamos un ejemplo de cómo usar esta función en un programa básico en Go:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hola mundo"
	capitalizedStr := strings.Title(str)
	fmt.Println(capitalizedStr)
}
```
**Salida:**
```
Hola Mundo
```

Como se puede ver en el código, primero importamos el paquete `strings` para poder utilizar la función `Title()`. Luego, definimos una cadena llamada `str` con el valor "hola mundo". Por último, asignamos a la variable `capitalizedStr` el resultado de aplicar la función `Title()` a `str` y la imprimimos en la consola.

## Profundizando en la capitalización de cadenas
Es importante mencionar que la función `strings.Title()` capitalizará no solo la primera letra de la cadena, sino también la primera letra de cada palabra en la cadena. Por ejemplo, si tenemos el siguiente código:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "hola mundo, bienvenido a Go"
	capitalizedStr := strings.Title(str)
	fmt.Println(capitalizedStr)
}
```

La salida será:
```
Hola Mundo, Bienvenido A Go
```

Además, es importante tener en cuenta que la función `Title()` solo capitalizará letras en minúscula. Si una palabra ya tiene la primera letra en mayúscula, no se modificará. Por ejemplo:
```
str := "Hello world"
capitalizedStr := strings.Title(str)
fmt.Println(capitalizedStr)
```

La salida seguirá siendo la misma: "Hello World".

## Ver también
- [Documentación oficial de la función `strings.Title()` en Go](https://golang.org/pkg/strings/#Title)
- [Cómo manipular cadenas en Go](https://www.tutorialspoint.com/go/go_strings.htm)
- [Más ejemplos de funciones para manipular cadenas en Go](https://gobyexample.com/string-functions)