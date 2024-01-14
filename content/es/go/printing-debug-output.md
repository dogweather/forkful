---
title:    "Go: Imprimiendo salida de depuración"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué imprimir la salida de depuración es importante en Go 

Imprimir la salida de depuración es una herramienta crucial en el proceso de desarrollo en Go. Nos permite ver detalladamente información sobre el funcionamiento de nuestro código mientras se está ejecutando, lo que nos ayuda a identificar y corregir errores de manera más eficiente.

## Cómo imprimir la salida de depuración en Go

Para imprimir la salida de depuración en Go, utilizamos la función `fmt.Printf()` y pasamos la información que queremos imprimir como argumento. Por ejemplo:

```Go
package main 

import "fmt"

func main() {
   nombre := "Juan" 
   edad := 25 

   fmt.Printf("Hola %s, tienes %d años", nombre, edad)
}
```

El resultado de este ejemplo sería:

```
Hola Juan, tienes 25 años
```

## Profundizando en la salida de depuración en Go

Además de sólo imprimir valores, también podemos utilizar la función `fmt.Printf()` para imprimir información sobre el tipo de dato o el formato de un valor. Por ejemplo:

```Go
package main 

import "fmt"

func main() {
   numero := 10
   peso := 67.5

   fmt.Printf("El tipo de dato de %d es %T\n", numero, numero)
   fmt.Printf("El peso de una persona promedio es %.2f kg\n", peso)
}
```

El resultado de este ejemplo sería:

```
El tipo de dato de 10 es int 
El peso de una persona promedio es 67.50 kg 
```

## Ver también

- [Documentación de fmt en la documentación oficial de Go](https://golang.org/pkg/fmt/)
- [Tutorial de Go en español](https://www.tutorialesprogramacionya.com/goya/index.php?cat=49)
- [Videos de introducción a Go en español](https://www.youtube.com/playlist?list=PLw8RQJQ8K1ySN6j9CmCu31r4UNUUMbcn0)