---
title:                "Usando un depurador"
date:                  2024-01-26T03:49:08.853456-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Utilizar un depurador es como tener un GPS en la jungla del código; te guía hacia el origen del problema. Los programadores usan depuradores para avanzar paso a paso por su código, inspeccionar variables y entender el flujo, facilitando la captura de bugs y la optimización del rendimiento.

## Cómo hacerlo:
Go tiene una herramienta integrada para depuración llamada Delve (`dlv`). Para comenzar, instala Delve, escribe un programa simple en Go, y luego ejecútalo a través del depurador.

```Go
// Primero, instala Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Ejemplo de programa en Go, guardar como main.go
package main

import "fmt"

func main() {
    message := "¡Depurando con Delve!"
    fmt.Println(message)
}

// Ejecuta tu programa con Delve
// dlv debug

// Algunos comandos básicos de Delve:
// (dlv) break main.main // establece un punto de interrupción en la función main
// (dlv) continue // ejecuta hasta el punto de interrupción o la terminación del programa
// (dlv) step // avanza paso a paso por el programa
// (dlv) print message // imprime el valor actual de la variable 'message'
// (dlv) quit // salir de Delve
```

Ejecutar `dlv debug` inicia una sesión de depuración. Una vez que llegas a un punto de interrupción que has establecido, puedes avanzar paso a paso por tu programa y ver qué está sucediendo bajo el capó.

## Profundización
Históricamente, los programadores de Go han utilizado varias herramientas para depurar, como GDB (GNU Debugger), pero enfrentaron desafíos porque GDB no estaba adaptado para el tiempo de ejecución y las goroutines de Go. Delve llegó al rescate con un mejor soporte para las características únicas de Go.

Existen alternativas a Delve como `go-dbg`, e incluso soporte de depurador integrado dentro de IDEs como Visual Studio Code y GoLand, que se envuelven alrededor de Delve para una experiencia más amigable para el usuario.

En el lado de la implementación, Delve trabaja utilizando los paquetes `runtime` y `debug/gosym`, entre otros, para acceder e interpretar los símbolos del programa y la información en tiempo de ejecución de Go. Se actualiza constantemente para mantenerse al día con las nuevas características y versiones del lenguaje.

## Ver también
- Repo oficial de Delve: https://github.com/go-delve/delve
- Tutorial de Depurador de Go por el equipo de Go: https://golang.org/doc/gdb
- Depuración de Go en Visual Studio Code: https://code.visualstudio.com/docs/languages/go#_debugging