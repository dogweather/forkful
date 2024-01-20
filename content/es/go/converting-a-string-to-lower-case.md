---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué? 

Convertir una cadena a minúsculas significa cambiar todos los caracteres alfabéticos de una cadena a su equivalente en minúsculas. Los programadores lo hacen para normalizar los datos y permitir comparaciones de cadena insensibles a mayúsculas y minúsculas.

## Cómo hacerlo:

En el lenguaje de programación Go, el paquete `strings` proporciona la función `ToLower` para convertir una cadena a minúsculas. Aquí tienes un ejemplo y su salida:

```Go
package main
import (
	"fmt"
	"strings"
)

func main() {
	s := "Hola Mundo"
	sLower := strings.ToLower(s)
	fmt.Println(sLower)
}
```

La salida será `hola mundo`.

## Profundizando:

### Contexto Histórico:

La necesidad de convertir cadenas a minúsculas aparece con frecuencia, en la comparación de cadenas de texto, en la normalización de las entradas de los usuarios, e incluso en la normalización de datos en base de datos.

### Alternativas:

Una posible alternativa a usar `strings.ToLower` es implementar tu propia función, aunque en la mayoría de los casos, utilizar la función proporcionada por el paquete `strings` será más eficiente y menos propenso a errores.

### Detalles de Implementación:

La función `ToLower` trabaja recorriendo la cadena de caracteres, y para cada carácter que es una letra mayúscula, convierte el carácter a minúsculas. Este método no modifica los caracteres que ya son minúsculas o que no son letras.

## Ver también:

Documentación del paquete `strings` en Go: https://pkg.go.dev/strings

Un interesante hilo en Go Forum sobre la comparación de cadenas: https://forum.golangbridge.org/t/string-comparison/5784