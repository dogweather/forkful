---
title:                "Calculando la longitud de una cadena"
date:                  2024-01-20T17:47:26.165499-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando la longitud de una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Encontrar la longitud de una cadena significa contar cuántos caracteres contiene. Esto es clave para tareas como validar entradas, limitar texto en interfaces de usuario y muchas otras operaciones de manipulación de cadenas.

## Cómo hacerlo:

En Go, usamos la función `len()` para obtener la longitud de una cadena. Aquí te muestro cómo:

```go
package main

import "fmt"

func main() {
    s := "¡Hola, mundo!"
    longitud := len(s)
    fmt.Println("Longitud de la cadena:", longitud)
}
```

Salida esperada:

```
Longitud de la cadena: 14
```

Notarás que cuenta los caracteres, incluyendo espacios y signos de puntuación.

## Análisis Profundo:

Históricamente, la función `len()` ha sido parte del lenguaje Go desde sus inicios, dada la necesidad frecuente de determinar el tamaño de diferentes estructuras de datos. En el contexto de las cadenas, `len()` devuelve la cantidad de bytes y no necesariamente de caracteres, lo cual es importante porque Go usa UTF-8 donde un solo carácter puede tener más de un byte.

Como alternativa, puedes usar `utf8.RuneCountInString(s)` del paquete `unicode/utf8` para contar los caracteres Unicode de forma correcta, especialmente para idiomas con caracteres que ocupan más de un byte.

Ejemplo con `utf8.RuneCountInString`:

```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    s := "¡Hola, mundo!"
    longitud := utf8.RuneCountInString(s)
    fmt.Println("Cantidad de caracteres Unicode:", longitud)
}
```

Salida esperada:

```
Cantidad de caracteres Unicode: 13
```

Aquí, vemos que el resultado es diferente porque `¡` se considera un carácter Unicode, mostrando la importancia de escoger la función adecuada según el contexto.

## Ver También:

- Documentación oficial de Go para la función `len()`: https://pkg.go.dev/builtin#len
- Paquete unicode/utf8 de Go: https://pkg.go.dev/unicode/utf8
- Una guía sobre strings en Go: https://blog.golang.org/strings

Recuerda, escoge sabiamente cómo contar los caracteres según lo que necesites, y práctica con ejemplos para entender cómo se comportan estas funciones con diferentes tipos de cadenas. ¡Buena suerte!
