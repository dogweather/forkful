---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:55.899156-07:00
description: "Usar un depurador en la programaci\xF3n Go implica emplear herramientas\
  \ o caracter\xEDsticas para inspeccionar y modificar el estado de un programa en\
  \ ejecuci\xF3n\u2026"
lastmod: 2024-02-19 22:05:17.119815
model: gpt-4-0125-preview
summary: "Usar un depurador en la programaci\xF3n Go implica emplear herramientas\
  \ o caracter\xEDsticas para inspeccionar y modificar el estado de un programa en\
  \ ejecuci\xF3n\u2026"
title: Utilizando un depurador
---

{{< edit_this_page >}}

## Qué y Por Qué?

Usar un depurador en la programación Go implica emplear herramientas o características para inspeccionar y modificar el estado de un programa en ejecución para entender su comportamiento o diagnosticar problemas. Los programadores hacen esto para encontrar y corregir errores de manera eficiente, optimizar el rendimiento y asegurar la corrección de su código.

## Cómo hacerlo:

Go proporciona una facilidad incorporada para la depuración llamada `delve`. Es una herramienta de depuración completa que te permite ejecutar programas Go paso a paso, inspeccionar variables del programa y evaluar expresiones.

Para comenzar, primero debes instalar `delve`. Puedes hacerlo ejecutando:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Ahora, vamos a depurar un simple programa Go. Considera un programa `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Depurando en Go"
    fmt.Println(message)
}
```

Para comenzar a depurar este programa, abre un terminal en el directorio del proyecto y ejecuta:

```shell
dlv debug
```

Este comando compila el programa con optimizaciones deshabilitadas (para mejorar la experiencia de depuración), lo inicia y adjunta un depurador a él.

Una vez que `delve` está en ejecución, estás en el shell interactivo del depurador. Aquí hay algunos comandos básicos:

- `break main.main` establece un punto de interrupción en la función `main`.
- `continue` reanuda la ejecución del programa hasta que se alcanza un punto de interrupción.
- `print message` imprimirá el valor de la variable `message`.
- `next` avanza la ejecución del programa a la siguiente línea.
- `quit` sale del depurador.

La salida al alcanzar el punto de interrupción e imprimir la variable podría verse así:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Depurando en Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Depurando en Go"
```

Usando estos comandos, puedes avanzar por tu programa, inspeccionando el estado a medida que avanzas para entender cómo se comporta e identificar cualquier problema.

## Estudio Profundo

La elección de `delve` como la herramienta de depuración de elección para Go sobre herramientas tradicionales como GDB (GNU Debugger) se debe principalmente a la naturaleza del modelo de ejecución y el tiempo de ejecución de Go. GDB no fue diseñado inicialmente teniendo en mente el tiempo de ejecución de Go, lo que hace que `delve` sea una opción más adecuada para los desarrolladores de Go. `Delve` está específicamente diseñado para Go, ofreciendo una experiencia de depuración más intuitiva para las goroutines de Go, canales y otros constructos específicos de Go.

Además, `delve` soporta una amplia gama de características más allá de las ofrecidas por GDB básico al trabajar con programas Go. Estas incluyen, pero no se limitan a: adjuntar a procesos en ejecución para depuración; puntos de interrupción condicionales; y evaluando expresiones complejas que pueden involucrar primitivas de concurrencia de Go.

Aunque `delve` es el depurador elegido por muchos desarrolladores de Go, vale la pena mencionar que la cadena de herramientas de Go también incluye formas de soporte de depuración más ligeras, como la herramienta incorporada `pprof` para perfilado y la herramienta `trace` para visualización de concurrencia. Estas herramientas a veces pueden proporcionar una vía más rápida o de más alto nivel para diagnosticar problemas de rendimiento del programa o errores de concurrencia, lo que podría ser complementario o incluso preferible dependiendo del contexto de la depuración.
