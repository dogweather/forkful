---
title:                "Extracción de subcadenas"
date:                  2024-01-20T17:45:43.271637-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extraer subcadenas es el acto de seleccionar partes específicas de un texto. Lo hacemos cuando queremos analizar, transformar o validar datos de manera eficiente.

## How to:
En Go, usamos principalmente `slice` para extraer subcadenas, así:

```go
package main

import (
	"fmt"
)

func main() {
	texto := "Hola mundo!"
	subcadena := texto[1:5]  // toma los caracteres del índice 1 al 4
	fmt.Println(subcadena)   // Imprime "ola "
}
```
Output:
´´´
ola
´´´

Aquí, el índice `1` es el inicio (incluido) y el `5` es el final (excluido).
Si quieres desde el comienzo hasta un punto usa `[:indice]`, y para ir desde un punto hasta el final usa `[indice:]`.

## Deep Dive
La historia de las subcadenas en Go es sencilla: desde sus comienzos, el lenguaje ofreció esta funcionalidad. En términos de alternativas, en Go, aparte de `slicing`, puedes usar el paquete `strings` para más funciones relacionadas con texto. Internamente, extraer subcadenas es ligero en Go, ya que la subcadena usa la misma memoria que la cadena original y solo cambia los índices de inicio y fin.

## See Also
- Documentación oficial de Go sobre `string`: https://golang.org/pkg/strings/
- Una guía sobre el paquete `strings`: https://www.calhoun.io/6-tips-for-using-strings-in-go/
- Tutorial sobre manejo de cadenas en Go: https://tour.golang.org/moretypes/15