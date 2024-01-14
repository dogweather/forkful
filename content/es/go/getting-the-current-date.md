---
title:    "Go: Obteniendo la fecha actual"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

La obtención de la fecha actual es una de las tareas más comunes en cualquier programa. Ya sea para registrar la fecha de creación de un archivo o para mostrar la fecha actual en una aplicación, es necesario saber cómo obtener la fecha actual en Go.

## Cómo hacerlo

En Go, la forma más sencilla de obtener la fecha actual es utilizando la función `Now()` de la librería `time`. Esta función devuelve un objeto `Time` que contiene la fecha y hora actuales. Veamos un ejemplo:

```Go
import (
    "fmt"
    "time"
)

func main() {
    fechaActual := time.Now()
    fmt.Println("La fecha actual es:", fechaActual)
}
```

La salida de este código sería:

```
La fecha actual es: 2021-07-16 15:30:00.000000001 +0000 UTC m=+0.000000001
```

Podemos mejorar la visualización utilizando la función `Format()` para especificar el formato de la fecha que queremos obtener. Por ejemplo, si solo queremos mostrar el día, mes y año, podemos hacer lo siguiente:

```Go
fechaActual := time.Now()
fechaFormateada := fechaActual.Format("02-01-2006")
fmt.Println("La fecha actual es:", fechaFormateada)
```

La salida sería:

```
La fecha actual es: 16-07-2021
```

Existen varios formatos disponibles para utilizar en la función `Format()`, como por ejemplo `January 2, 2006` para mostrar la fecha en formato largo.

## Profundizando

Detrás de la función `Now()` existe un proceso más complejo que involucra el uso del reloj local del sistema operativo y la conversión a un objeto `Time` en Go. También es importante tener en cuenta que, al ser Go un lenguaje compilado, la fecha actual se obtiene en el momento de la compilación y no en el momento de la ejecución.

Para obtener una fecha específica, se pueden utilizar otras funciones como `Date()` o `Parse()` de la librería `time`. Además, es importante tener en cuenta que el uso de la función `Now()` puede variar dependiendo del sistema operativo en el que se esté ejecutando el programa.

## Ver también

- [Documentación oficial de la librería time en Go](https://golang.org/pkg/time/)
- [Tutorial de Go: Fecha y tiempo](https://golangbot.com/time/)
- [Ejemplos prácticos de uso de la librería time en Go](https://golangdocs.com/golang-time-example)