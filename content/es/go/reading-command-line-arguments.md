---
title:    "Go: Argumentos de línea de comando en la lectura"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué
Si estás comenzando a aprender Go y quieres aumentar tus habilidades de programación, leer argumentos de línea de comando es una herramienta útil para tener en tu cinturón de herramientas. Ahora que ya has aprendido los conceptos básicos, es hora de profundizar en como puedes usar esta funcionalidad en tus proyectos.

## Cómo hacerlo
Para leer los argumentos de línea de comando en Go, necesitamos importar el paquete "os". Luego, podemos usar la función "Args" para obtener una lista de todos los argumentos ingresados en la línea de comando. Por ejemplo:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args
	fmt.Println(args)
}
```

Si ejecutamos este código y pasamos algunos argumentos en la línea de comando, como `go run main.go arg1 arg2`, obtendremos el siguiente resultado:

`[main.go arg1 arg2]`

Podemos acceder a cada argumento individualmente usando su índice en la lista. Por ejemplo, `args[0]` sería el nombre del archivo compilado, `args[1]` sería el primer argumento ingresado, `args[2]` el segundo argumento, y así sucesivamente.

## Profundizando
Además de obtener una lista de argumentos, también podemos usar la función "Flag" del paquete "flag" para especificar argumentos específicos que necesitamos para nuestro programa. Por ejemplo, si queremos que nuestro programa acepte un argumento de tipo "string", podemos usar la función "String" de "flag" para definir una variable que almacene ese argumento. Luego, en nuestro programa, podemos acceder a ese argumento usando el nombre de esa variable. Por ejemplo:

```Go
package main

import (
	"flag"
	"fmt"
)

func main() {
	var myArg string
	flag.StringVar(&myArg, "myArg", "", "This is a string argument")
	flag.Parse()
	fmt.Println(myArg)
}
```

Al ejecutar este código y pasar el argumento de línea de comando `-myArg=test`, obtendremos el siguiente resultado:

`test`

## Ver también
- [Documentación del paquete "os" en Go](https://golang.org/pkg/os/)
- [Documentación del paquete "flag" en Go](https://golang.org/pkg/flag/)