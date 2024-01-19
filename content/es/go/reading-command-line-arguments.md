---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lección Práctica de Go: Leyendo Argumentos de la Línea de Comandos 

## ¿Qué y por qué?

Los argumentos de la línea de comandos son entradas que los usuarios pueden introducir cuando corren tu programa. Estos son esenciales porque permiten una mayor personalización y flexibilidad en tus aplicaciones.

## Cómo hacerlo:

Go hace muy sencillo la lectura de argumentos de la línea de comandos con ayuda del paquete `os`.

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	argsWithProg := os.Args
	argsWithoutProg := os.Args[1:]

	arg := os.Args[3]

	fmt.Println(argsWithProg)
	fmt.Println(argsWithoutProg)
	fmt.Println(arg)
}
```
Cuando corres este programa con `go run main.go primer_arg segundo_arg tercer_arg` la salida será:

```Go
[main.go primer_arg segundo_arg tercer_arg]
[primer_arg segundo_arg tercer_arg]
tercer_arg
```

## Inmersión Profunda:

Go, lanzado por Google en 2007, tomó una posición única para manejar los argumentos de la línea de comandos, diferente a lenguajes como C y Perl. El índice 0 contiene el nombre del programa, que es una convención usada por muchas lenguajes de programación Unix.

Además de `os.Args`, existen alternativas como el paquete `flag` para un manejo más avanzado de los argumentos. `flag` permite que argumentos con nombres, algo similar a las opciones en los comandos de Linux.

`os.Args` es un slice y por tanto, puedes usar todas las funciones y métodos disponibles para los slices en Go. Como en el ejemplo mostrado arriba, `os.Args[1:]` te dará todos los argumentos excepto el nombre del programa.

## Ver También:

Algunos enlaces útiles si quieres seguir profundizando en este tema:

- Documentación oficial de Go para el paquete os: https://golang.org/pkg/os/
- Paquete flag de Go: https://golang.org/pkg/flag/
- Guía de Gotuts sobre cómo manejar argumentos de la línea de comandos: https://gobyexample.com/command-line-arguments