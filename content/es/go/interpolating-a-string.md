---
title:                "Interpolación de cadenas de texto"
date:                  2024-01-20T17:51:06.293922-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolación de cadenas de texto"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Interpolar una cadena significa insertar valores de variables dentro de ella. Los programadores lo hacen para construir strings dinámicamente, facilitando la creación de mensajes personalizados y la salida de datos de forma legible.

## Cómo hacerlo:

En Go, la forma más común de interpolar cadenas es usando `fmt.Sprintf` o el paquete `strings.Builder`. Aquí te muestro cómo:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Usando fmt.Sprintf
	nombre := "Mundo"
	mensaje := fmt.Sprintf("¡Hola, %s!", nombre)
	fmt.Println(mensaje) // Salida: ¡Hola, Mundo!

	// Usando strings.Builder
	var sb strings.Builder
	sb.WriteString("¡Hola, ")
	sb.WriteString(nombre)
	sb.WriteString("!")
	fmt.Println(sb.String()) // Salida: ¡Hola, Mundo!
}
```

## Profundizando

Históricamente, Go ha favorecido un enfoque de bajo cerimonial para tareas comunes como la interpolación de cadenas, evitando añadir sintaxis específica como en otros lenguajes (por ejemplo, el uso de `${}` en JavaScript). Antes del `fmt` o `strings.Builder`, la concatenación manual era el camino a seguir, pero era menos eficiente.

Hablando de eficiencia, `strings.Builder` es generalmente más rápido y usa menos memoria cuando estás concatenando o interpolando muchas cadenas. Esto se debe a que minimiza las asignaciones de memoria al trabajar con búferes internamente.

En cuanto a alternativas, paquetes de terceros como `template` ofrecen soluciones más robustas y flexibles para casos de uso más complejos, permitiendo crear plantillas que pueden evaluarse con distintos datos.

## Ver También

- Documentación oficial de `fmt`: https://pkg.go.dev/fmt
- Documentación oficial de `strings`: https://pkg.go.dev/strings
- Blog de Go sobre eficiencia de strings: https://blog.golang.org/strings
- Paquete `text/template`: https://pkg.go.dev/text/template
