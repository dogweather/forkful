---
title:    "Go: Convirtiendo una cadena a minúsculas"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué

La conversión de una cadena de texto a minúsculas puede ser una tarea muy útil cuando se trabaja con datos de entrada de usuarios o para fines de normalización de datos. Conocer cómo realizar esta conversión en Go puede mejorar la precisión de las búsquedas o comparaciones de cadenas de texto.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Go, simplemente se utiliza la función `strings.ToLower()`. A continuación se muestra un ejemplo de código que convierte una cadena de texto a minúsculas y luego la imprime en pantalla:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	texto := "¡HOLA A TODOS!"
	fmt.Printf("Texto original: %s\n", texto)
	texto = strings.ToLower(texto)
	fmt.Printf("Texto convertido a minúsculas: %s\n", texto)
}
```

La salida de este código será:

```
Texto original: ¡HOLA A TODOS!
Texto convertido a minúsculas: ¡hola a todos!
```

## Profundizando

Es importante tener en cuenta que la conversión a minúsculas en Go utiliza unicode, lo que significa que también convertirá caracteres especiales de otros idiomas a su equivalente en minúsculas. Por ejemplo, la vocal Ñ en español se convertirá a ñ. Además, la función `strings.ToLower()` también puede manejar correctamente caracteres acentuados, como é, á o ô.

Otra forma de convertir una cadena de texto a minúsculas es utilizando la función `strings.Map()`. Esta función permite especificar una función de mapeo que se aplicará a cada carácter de la cadena. A continuación, se muestra un ejemplo de cómo utilizar esta función para convertir una cadena de texto a minúsculas:

```
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	texto := "¡HOLA A TODOS!"
	fmt.Printf("Texto original: %s\n", texto)
	texto = strings.Map(unicode.ToLower, texto)
	fmt.Printf("Texto convertido a minúsculas: %s\n", texto)
}
```

La salida de este código será la misma que en el ejemplo anterior.

## Ver también

- Documentación oficial de la función `strings.ToLower()` en el sitio web de Go: https://golang.org/pkg/strings/#ToLower
- Documentación oficial de la función `strings.Map()` en el sitio web de Go: https://golang.org/pkg/strings/#Map
- Ejemplos de uso de cadenas de texto en Go: https://blog.golang.org/strings