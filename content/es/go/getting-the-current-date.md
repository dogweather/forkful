---
title:                "Go: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual en Go

Una de las tareas más comunes al programar es obtener la fecha y hora actuales. Ya sea para mostrarla en una interfaz de usuario o para realizar cálculos con fechas, es una funcionalidad básica en cualquier aplicación. En este artículo, aprenderemos cómo obtener la fecha actual en Go de forma sencilla y eficiente.

## Cómo hacerlo

En Go, podemos obtener la fecha actual utilizando la función `Now()` del paquete `time`. Esta función devuelve un objeto `Time` que contiene la fecha, hora y zona horaria actuales. Veamos un ejemplo:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    today := time.Now()
    fmt.Println("Fecha actual: ", today)
}
```

El código anterior nos dará como resultado algo similar a: `Fecha actual:  2021-03-26 15:30:00.7002462 +0000 UTC m=+0.000000001`. Podemos utilizar los métodos `Day()`, `Month()` y `Year()` del objeto `Time` para obtener la fecha en un formato más legible. Por ejemplo:

```Go
fmt.Printf("Hoy es %d de %s del %d", today.Day(), today.Month(), today.Year())
```

Esto nos dará como resultado: `Hoy es 26 de March del 2021`.

También podemos utilizar el método `Format()` para obtener la fecha en un formato específico. Por ejemplo:

```Go
//Obtener la fecha en formato "día/mes/año"
fmt.Println("Fecha actual: ", today.Format("02/01/2006"))
```

Este código nos dará como resultado: `Fecha actual: 26/03/2021`.

## Profundizando

Go cuenta con una serie de métodos y funciones que nos permiten trabajar con fechas de forma eficiente. Por ejemplo, podemos sumar o restar días, meses o años utilizando el método `Add()`, o podemos comparar dos fechas utilizando el método `Before()` o `After()`. También podemos obtener la diferencia de tiempo entre dos fechas con el método `Sub()`.

Además, Go nos permite trabajar con diferentes zonas horarias utilizando el paquete `time`, lo que resulta muy útil en aplicaciones que deben manejar fechas y horas en diferentes lugares del mundo.

En resumen, Go cuenta con una serie de herramientas para trabajar con fechas de forma sencilla y eficiente, permitiéndonos realizar tareas comunes como obtener la fecha actual o realizar cálculos con fechas de forma rápida y precisa.

## Ver también

- [Paquete time en la documentación oficial de Go](https://golang.org/pkg/time/)
- [Tutorial: cómo trabajar con fechas en Go](https://yourbasic.org/golang/time-format-parse-string-timestamp/)
- [Ejemplo práctico: cómo utilizar diferentes zonas horarias en Go](https://dev.to/nicolasiensen/managing-timezones-in-golang-with-standard-library-5a2)