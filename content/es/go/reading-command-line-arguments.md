---
title:                "Go: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Leer argumentos de la línea de comandos es una habilidad importante para cualquier programador de Go. Nos permite interactuar con nuestro programa de una manera práctica y eficiente. ¡Sigue leyendo para descubrir cómo hacerlo!

## Cómo hacerlo

En Go, podemos usar el paquete `flag` para leer argumentos de la línea de comandos. Primero, importamos el paquete en nuestro programa:

```Go
import "flag" 
```
Luego, podemos definir las variables que almacenarán nuestros argumentos utilizando la función `flag.String`:

```Go
var direccion = flag.String("direccion", "default", "La dirección del servidor")
```
En este ejemplo, `direccion` se refiere al nombre del argumento, `default` es el valor predeterminado que se utilizará si no se proporciona un valor y `La dirección del servidor` es una breve descripción del argumento.

Finalmente, en el `main`, podemos usar la función `flag.Parse()` para leer los argumentos proporcionados por el usuario:

```Go
func main() {
  flag.Parse()
  // código para utilizar el argumento
}
```
Si quisiéramos ver el valor del argumento en nuestro programa, podríamos imprimirlo de esta manera:
```Go
fmt.Println("La dirección del servidor es:", *direccion)
```
Entonces, si ejecutamos nuestro programa de esta manera `go run main.go -direccion=localhost`, el resultado sería:
```Go
La dirección del servidor es: localhost
```

## Profundizando

Hay varias opciones que podemos utilizar en la función `flag.String` para personalizar nuestros argumentos. Podemos especificar un tipo de datos diferente, como `int` o `bool`, y también podemos proporcionar un conjunto de opciones en lugar de un solo valor.

Además, si queremos acceder a los argumentos en cualquier parte de nuestro programa, podemos definirlos fuera de la función `main`, lo que los convierte en variables globales.

Puedes encontrar más información sobre el paquete `flag` en la [documentación oficial de Go](https://golang.org/pkg/flag/).

## Ver también

- [Documentación oficial del paquete `flag`](https://golang.org/pkg/flag/)
- [Video tutorial sobre el manejo de argumentos de línea de comandos en Go](https://www.youtube.com/watch?v=-PxzBh-ClhE&t=548s)
- [Artículo sobre los argumentos de la línea de comandos en Go](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-go-es)