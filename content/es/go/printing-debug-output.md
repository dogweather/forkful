---
title:                "Go: Imprimiendo la salida de depuración"
simple_title:         "Imprimiendo la salida de depuración"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has encontrado con un error en tu código y no puedes entender por qué está sucediendo? ¡La impresión de la salida de depuración puede ser una herramienta muy útil en estas situaciones! Al imprimir información específica en puntos clave de tu código, puedes rastrear y solucionar errores más fácilmente.

## Cómo hacerlo
Para imprimir la salida de depuración en Go, puedes usar la función "fmt.Println()". Por ejemplo:

```Go
package main

import "fmt"

func main() {
  // Crea una variable
  nombre := "Juan"

  // Imprime la variable
  fmt.Println("El nombre es:", nombre)
}
```

La salida de este código sería: "El nombre es: Juan". También puedes utilizar la función "fmt.Printf()" para formatear la salida de manera más específica. Por ejemplo:

```Go
package main

import "fmt"

func main() {
  // Crea una variable
  edad := 22

  // Imprime la variable formateada
  fmt.Printf("La edad es: %d años", edad)
}
```

La salida de este código sería: "La edad es: 22 años". Puedes ver cómo el formato "%d" se reemplaza con el valor de la variable "edad".

## Profundizando
Además de la función "fmt", Go también cuenta con el paquete "log" que proporciona funcionalidades para imprimir la salida de depuración. Este paquete tiene diferentes funciones como "log.Print()", "log.Println()" y "log.Printf()". También puedes especificar el nivel de logeo (debug, info, warning, error) y la hora en que se imprime la salida.

Otra forma de imprimir la salida de depuración en Go es utilizando el debugger integrado en las principales IDE como Visual Studio Code o Goland. Este te permite visualizar variables, pausar la ejecución y rastrear errores de manera más eficiente.

## Ver también
- [Documentación oficial de debugging en Go](https://golang.org/doc/gdb) 
- [Tutorial de debugging en Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging) 
- [Tutorial de debugging en Goland](https://www.jetbrains.com/help/go/debugging-with-goland.html)