---
title:                "Comenzando un nuevo proyecto"
html_title:           "Go: Comenzando un nuevo proyecto"
simple_title:         "Comenzando un nuevo proyecto"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por qué

¡Hola a todos! Si estás aquí, es posible que estés considerando empezar un nuevo proyecto con Go. Y déjame decirte, ¡es una gran elección! Go es un lenguaje de programación moderno y altamente eficiente que está ganando popularidad en la comunidad de desarrollo. Ya sea que estés buscando crear una aplicación web, escribir herramientas de línea de comandos o construir sistemas de gran escala, Go es una excelente opción para llevar tus ideas al siguiente nivel.

## Cómo

Antes de sumergirnos en los detalles de cómo iniciar un nuevo proyecto en Go, es importante verificar que tengas instalado Go en tu computadora y que tengas un buen editor de texto (como Visual Studio Code o Atom) para escribir tu código. Una vez que estés listo, ¡vamos a empezar!

Para crear un nuevo proyecto en Go, primero debes crear un directorio en tu computadora donde almacenarás todos los archivos relacionados con tu proyecto. Luego, dentro de ese directorio, crea un archivo llamado "main.go". Este será nuestro punto de entrada para el código de nuestro proyecto. Ahora, dentro del archivo "main.go", escribe el siguiente código:

```Go
package main

import "fmt"

func main() {
	fmt.Println("¡Hola, mundo!")
}
```

¡Genial! Acabas de escribir tu primer programa en Go. Ahora, para ejecutarlo, abra una ventana de terminal, vaya al directorio donde creaste tu archivo "main.go" y escribe el siguiente comando:

```Go
go run main.go
```

Deberías ver el mensaje "¡Hola, mundo!" impreso en la terminal. ¡Felicidades, acabas de ejecutar con éxito tu primer programa en Go!

## Inmersión Profunda

Comenzar un nuevo proyecto en Go implica mucho más que simplemente escribir un "¡Hola, mundo!" en la terminal. Puedes utilizar la gran variedad de paquetes y herramientas disponibles en el lenguaje para llevar tus proyectos al siguiente nivel. Algunas de las características notables de Go incluyen su recolector de basura automático, concurrencia incorporada y una sintaxis concisa y legible.

Además, Go tiene una comunidad activa y en constante crecimiento, lo que significa que siempre puedes encontrar ayuda o recursos en línea para ayudarte a resolver cualquier problema que encuentres en tu proyecto. Así que no dudes en explorar y experimentar con diferentes paquetes y herramientas para encontrar las mejores soluciones para tu proyecto.

## Ver también

[The Go Programming Language](https://golang.org/)

[A Tour of Go](https://tour.golang.org/)

[Aprende Go con Ejercicios](https://github.com/golang-es/ejercicios)