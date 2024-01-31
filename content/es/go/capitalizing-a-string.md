---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
simple_title:         "Capitalizando una cadena de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Capitalizar un string significa convertir la primera letra de una palabra o todas las letras de un texto a mayúsculas. Los programadores lo hacen para estandarizar datos, mejorar la legibilidad o cumplir con requisitos específicos como nombres propios o títulos.

## Cómo hacerlo:
Go hace que capitalizar un string sea cosa de un segundo. Usamos el paquete `strings` para capitalizar sólo la primera letra o el paquete `unicode/utf8` si queremos manejar caracteres fuera del rango ASCII. Aquí hay ejemplos:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

func main() {
	// Capitalizar solo la primera letra
	phrase := "hola mundo"
	capitalized := strings.Title(strings.ToLower(phrase))
	fmt.Println(capitalized) // Salida: Hola Mundo

	// Capitalizar todas las letras
	upperCase := strings.ToUpper(phrase)
	fmt.Println(upperCase) // Salida: HOLA MUNDO

	// Capitalizar primera letra de un string UTF-8
	r, size := utf8.DecodeRuneInString(phrase)
	phrase = string(unicode.ToUpper(r)) + phrase[size:]
	fmt.Println(phrase) // Salida: Hola mundo
}
```

## Un poco más a fondo:
En el pasado, la capitalización se hacía a mano con tablas de conversión entre mayúsculas y minúsculas. Ahora, con Go, usamos las funciones ya integradas para evitar errores y ahorrar tiempo. Aunque `strings.Title()` puede parecer la opción obvia, hay que tener cuidado porque capitaliza cada palabra y no sólo la primera letra de un texto. Por otro lado, si solo necesitas convertir a mayúsculas la primera letra de toda la cadena y el resto a minúsculas, tendrás que manipular los caracteres manualmente, como en el tercer ejemplo, especialmente si estás trabajando con múltiples lenguajes o caracteres especiales.

## Ver también:
- Documentación oficial del paquete de `strings`: https://pkg.go.dev/strings
- Información sobre el paquete `unicode/utf8`: https://pkg.go.dev/unicode/utf8
- Tutorial detallado para entender UTF-8 en Go: https://blog.golang.org/strings
