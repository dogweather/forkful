---
title:                "Escribiendo en el error estándar"
html_title:           "Go: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¡Oye! ¿Qué es y por qué escribe a la salida de error estándar?

Es posible que hayas visto a los programadores escribir a la salida de error estándar y te preguntes ¿qué es esto y por qué lo hacen? Bueno, ¡sigue leyendo para descubrirlo!

## Cómo hacerlo:

Para escribir a la salida de error estándar en Go, debes usar la función ```Go fmt.Fprintln(os.Stderr, "¡Hola, mundo!") ``` Esta función toma dos argumentos, el primero es el destino donde se escribirá el mensaje (en este caso, la salida de error estándar) y el segundo es el mensaje en sí. Aquí hay un ejemplo de cómo se vería en un programa completo:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	fmt.Fprintln(os.Stderr, "¡Hola, mundo!")
}
```

El resultado de este programa sería:

```
¡Hola, mundo! 
```

## Inmersión profunda:

La práctica de escribir a la salida de error estándar es común entre los programadores, ya que es una forma fácil y rápida de imprimir mensajes de error en caso de que algo salga mal en el programa. En lugar de esperar a que el programa se bloquee o se cierre inesperadamente, los programadores pueden utilizar la salida de error estándar para mostrar mensajes de diagnóstico útiles.

Aunque la función ```fmt.Fprintln()``` es la forma recomendada de escribir a la salida de error estándar en Go, también es posible usar la función ```fmt.Fprintf()``` si se desea formatear el mensaje. Además de la salida de error estándar, también es posible escribir a la salida estándar o a un archivo utilizando estas funciones.

## Véase también:

- [Documentación de la función fmt.Fprintln() en Go](https://golang.org/pkg/fmt/#Fprintln)
- [Información sobre la salida de error estándar en Go](https://gobyexample.com/reading-files)
- [Tutorial completo sobre cómo imprimir a la salida de error estándar en Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-error-handling-in-golang-es)