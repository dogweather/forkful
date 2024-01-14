---
title:                "Go: Leyendo argumentos de línea de comando"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

La lectura de argumentos de línea de comandos es una habilidad esencial para cualquier programador de Go. Con esta técnica, podrás interactuar con tu programa en tiempo de ejecución y personalizar su comportamiento. Además, es una buena práctica de programación y te ayudará a escribir programas más robustos.

## Cómo hacerlo

El paquete "flag" de Go nos proporciona una manera fácil de leer argumentos de línea de comandos. Primero, importamos el paquete en nuestro programa:

```Go
import "flag"
```

Luego, podemos definir las variables donde queremos almacenar los argumentos:

```Go
var flag1 string
var flag2 bool
```

Después, usamos la función "flag.String()" o "flag.Bool()" para enlazar esas variables a los argumentos correspondientes:

```Go
flag.StringVar(&flag1, "flag1", "default", "Este es el primer argumento.")
flag.BoolVar(&flag2, "flag2", false, "Este es el segundo argumento.")
```

Finalmente, llamamos a la función "flag.Parse()" para leer los argumentos y asignarlos a nuestras variables:

```Go
flag.Parse()
```

Ahora podemos usar las variables "flag1" y "flag2" en nuestro programa para realizar acciones en función de los argumentos ingresados por el usuario.

## Profundizando más

El paquete "flag" también nos permite definir argumentos posicionales y establecer valores por defecto para ellos. Además, podemos usar el comando "-h" para mostrar un mensaje de ayuda con la descripción de todos los argumentos y su uso. También podemos establecer variables globales para almacenar los argumentos y acceder a ellos desde cualquier parte de nuestro programa.

## Ver también

- Documentación oficial del paquete "flag" en Go: https://golang.org/pkg/flag/
- Tutorial de lectura de argumentos de línea de comandos en en Go: https://www.golangprograms.com/command-line-arguments-in-golang.html
- Ejemplos de uso del paquete "flag" en Go: https://www.calhoun.io/5-tips-for-using-go-flags/