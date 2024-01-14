---
title:                "Go: Extrayendo subcadenas"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías extraer subcadenas en Go?

Extraer subcadenas es una operación común en la programación, especialmente cuando se trabaja con cadenas de texto largas. En Go, es una tarea sencilla que puede ahorrar tiempo y mejorar la eficiencia de tu código.

## Cómo extraer subcadenas en Go

Para extraer una subcadena en Go, utilizamos la función `Substr()` de la librería `strings` y le pasamos como argumentos la cadena original, el índice de inicio y la longitud de la subcadena deseada. Por ejemplo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "¡Hoy es un buen día para aprender Go!"
	substr := strings.Substr(str, 9, 8)
	fmt.Println(substr)
}
```

Este código imprimirá "un buen día", ya que empieza en el índice 9 y tiene una longitud de 8 caracteres.

También podemos extraer una subcadena desde un cierto índice hasta el final de la cadena utilizando la función `SubstrFrom()`:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "¡Hoy es un buen día para aprender Go!"
	substr := strings.SubstrFrom(str, 22)
	fmt.Println(substr)
}
```

Este código imprimirá "para aprender Go!", ya que empieza en el índice 22 y va hasta el final de la cadena.

Además de estas funciones, también tenemos la opción de utilizar `SubstrBetween()` para extraer una subcadena entre dos índices específicos.

## Un vistazo más profundo a la extracción de subcadenas

Ahora que sabemos cómo utilizar las funciones `Substr()`, `SubstrFrom()` y `SubstrBetween()`, profundicemos un poco más en cómo funcionan estas funciones.

La función `Substr()` toma tres argumentos: la cadena original, el índice de inicio y la longitud de la subcadena. El índice de inicio puede ser negativo, lo que significa que se contará desde el final de la cadena. Por ejemplo, si pasamos un índice de inicio de -5 en lugar de 5, extraería los últimos 5 caracteres de la cadena.

La función `SubstrFrom()` toma dos argumentos: la cadena original y el índice de inicio. En este caso, si el índice de inicio es negativo, se extraerán los caracteres desde ese índice hasta el final de la cadena.

La función `SubstrBetween()` también toma dos argumentos: la cadena original y dos índices. En este caso, ambos índices pueden ser negativos, lo que proporciona una mayor flexibilidad al extraer subcadenas.

En resumen, la extracción de subcadenas en Go es una tarea sencilla gracias a las funciones `Substr()`, `SubstrFrom()` y `SubstrBetween()`. Con solo unos pocos parámetros, podemos extraer fácilmente partes específicas de una cadena de texto.

## Ver también

- Documentación oficial de la librería `strings` en Go: https://golang.org/pkg/strings/
- Tutorial en línea sobre la extracción de subcadenas en Go: https://www.tutorialspoint.com/golang/golang_strings.htm
- Video de YouTube que muestra ejemplos prácticos de extracción de subcadenas en Go: https://www.youtube.com/watch?v=XXnZC9ugUls