---
title:                "Go: Convertir una cadena a minúsculas"
simple_title:         "Convertir una cadena a minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Go?

Hay muchas razones por las que alguien puede querer convertir una cadena a minúsculas en Go. Una de las razones más comunes es para comparar dos cadenas sin importar las mayúsculas y minúsculas. También puede ser útil para normalizar datos o limpiar entradas de usuario.

## Cómo hacerlo en Go

Para convertir una cadena a minúsculas en Go, utilizamos la función `strings.ToLower()` del paquete `strings`. Esta función toma una cadena como parámetro y devuelve una nueva cadena en minúsculas.

Veamos un ejemplo de cómo usar esta función en código Go:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Ejemplo de CADENA EN mayúsculas"
	fmt.Println(strings.ToLower(str))
}
```

El resultado de este código será:

```Go
ejemplo de cadena en mayúsculas
```

## Profundizando en la conversión a minúsculas

Algunas cosas a tener en cuenta al convertir una cadena a minúsculas en Go son que no solo convierte caracteres en mayúsculas a minúsculas, sino que también cambia caracteres como Ñ, Ü o Ç a sus equivalentes en minúsculas (ñ, ü, ç).

Si queremos hacer una comparación de igualdad entre dos cadenas sin tener en cuenta las mayúsculas y minúsculas, también podemos usar la función `strings.EqualFold()` del paquete `strings`. Esta función comparará dos cadenas en minúsculas y devolverá true si son iguales, incluso si se escribieron con diferentes casos.

## Ver también

- Documentación oficial de la función ToLower en el paquete `strings`: https://golang.org/pkg/strings/#ToLower
- Documentación oficial de la función EqualFold en el paquete `strings`: https://golang.org/pkg/strings/#EqualFold