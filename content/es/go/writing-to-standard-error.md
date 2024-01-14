---
title:    "Go: Escribiendo en el error estándar"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué

Escribir en la salida de error estándar (standard error) es una herramienta importante para cualquier programador de Go. Esto te permite imprimir mensajes de error y depurar tu código de manera efectiva. En esta publicación, exploraremos cómo escribir en la salida de error estándar en Go y por qué es una práctica valiosa.

## Cómo

Para escribir en la salida de error estándar en Go, simplemente necesitas importar el paquete "fmt" y usar la función "Fprintf" con "os.Stderr" como primer argumento. Aquí hay un ejemplo:

```Go
import "fmt"
import "os"

func main() {
    fmt.Fprintf(os.Stderr, "Este es un mensaje de error")
}
```

La salida de este código sería:

```
Este es un mensaje de error
```

Puedes usar el formato de "Printf" para personalizar tu mensaje de error aún más. Por ejemplo:

```Go
func main() {
    fmt.Fprintf(os.Stderr, "Ha ocurrido un %s en la línea %d", "error", 10)
}
```

La salida de este código sería:

```
Ha ocurrido un error en la línea 10
```

## Deep Dive

Mientras que la salida de error estándar es una herramienta útil para imprimir mensajes de error, también es importante tener en cuenta su función en la depuración de código. Al escribir mensajes de error en la salida estándar, puedes identificar fácilmente dónde en tu código se ha producido un error y tomar medidas para solucionarlo.

Además, la salida de error estándar se imprime en la consola en tiempo real, a diferencia de la salida estándar que se almacena en búfer y se imprime al final de la ejecución del programa. Esto significa que si tu programa se está ejecutando durante un período de tiempo largo, puedes ver los mensajes de error en tiempo real y solucionarlos mientras el programa aún está en funcionamiento.

## Ver También

- [Documentación de Fprintf en Go](https://golang.org/pkg/fmt/#Fprintf)
- [Escribiendo a la salida de error estándar en Go](https://golangbyexample.com/write-to-standard-error-go/)
- [Cómo depurar en Go](https://blog.golang.org/debugging-with-gdb)