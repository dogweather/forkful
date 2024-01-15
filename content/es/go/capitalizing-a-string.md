---
title:                "Convertir una cadena a mayúsculas"
html_title:           "Go: Convertir una cadena a mayúsculas"
simple_title:         "Convertir una cadena a mayúsculas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué capitalizar una cadena en Go?

La capitalización de una cadena en Go puede ser útil en varias situaciones, como en la creación de títulos o nombres con las primeras letras en mayúscula. También puede ser útil para normalizar la entrada del usuario y garantizar que todas las palabras comiencen con mayúsculas.

## Cómo hacerlo
La función `Title` de la biblioteca `strings` en Go permite capitalizar una cadena. A continuación, se muestra un ejemplo de cómo utilizarla en un programa:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    cadena := "esta es una prueba"
    cadenaCapitalizada := strings.Title(cadena)
    fmt.Println(cadenaCapitalizada)
}
```

La salida de este programa sería "Esta Es Una Prueba". Como se puede ver, la primera letra de cada palabra ha sido cambiada a mayúscula.

## Más detalles

La función `Title` de la biblioteca `strings` en Go utiliza reglas específicas para capitalizar una cadena. Por ejemplo, las palabras en minúsculas como "a", "de" o "del" no se capitalizarán a menos que sean la primera palabra en la cadena. También tiene en cuenta las letras acentuadas y caracteres especiales al capitalizar una cadena.

Si se necesita una forma más personalizada de capitalizar una cadena, se puede usar la función `Map` de la biblioteca `strings` para crear una función que capitalice de acuerdo a ciertos criterios personalizados.

## Ver también
- Documentación oficial de la función `Title` de `strings`: https://golang.org/pkg/strings/#Title
- Ejemplos adicionales de capitalización en Go: https://golangbyexample.com/strings-golang/
- Otras funciones útiles de la biblioteca `strings` en Go: https://www.callicoder.com/golang-strings-cheat-sheet/