---
title:                "Go: Calculando una fecha en el futuro o pasado"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas futuras o pasadas en Go?

A menudo, en la programación, necesitamos trabajar con fechas y realizar cálculos basados en estas. En Go, podemos utilizar diferentes funciones y métodos para calcular fechas futuras o pasadas de manera sencilla y eficiente. A continuación, te explicaremos cómo hacerlo.

## Cómo hacerlo en Go

Para calcular una fecha futura o pasada en Go, podemos utilizar la función `AddDate` del paquete `time`. Esta función toma 3 parámetros: el año, el mes y el día que queremos agregar o restar a la fecha actual. Veamos un ejemplo de cómo utilizar esta función en código:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    fechaActual := time.Now()
    fechaFutura := fechaActual.AddDate(0, 3, 0)

    fmt.Println("Hoy es:", fechaActual.Format("02/01/2006"))
    fmt.Println("Dentro de 3 meses será:", fechaFutura.Format("02/01/2006"))
}
```
Este código nos mostrará en consola la fecha actual y la fecha futura calculada, con un formato específico de día/mes/año. En este caso, estamos agregando 3 meses a la fecha actual, pero también podemos restar si así lo deseamos.

## Profundizando en el cálculo de fechas en Go

Además de la función `AddDate`, podemos utilizar otras funciones para calcular fechas en Go. Por ejemplo, `Date` nos permite crear una fecha específica con año, mes y día, y `Parse` nos permite convertir una cadena de texto en una fecha válida. También podemos utilizar el método `Before` para comparar dos fechas y determinar si una es anterior a la otra.

Es importante tener en cuenta que las fechas en Go se manejan de manera diferente a otros lenguajes de programación, por lo que debemos asegurarnos de utilizar correctamente los parámetros y formatos requeridos en cada función.

## Ver también

- [Documentación oficial de Go sobre el paquete time](https://golang.org/pkg/time/)
- [Tutorial de cálculo de fechas en Go](https://tutorialedge.net/golang/go-working-with-dates/)
- [Ejemplos de cálculo de fechas en Go](https://gocodecloud.com/blog/2020/07/06/working-with-go-time/)

¡Ahora que sabes calcular fechas en Go, podrás aplicar este conocimiento en tus proyectos y trabajar de manera más eficiente con el manejo de fechas! Recuerda siempre consultar la documentación oficial y revisar ejemplos para asegurarte de utilizar correctamente las funciones y métodos de Go.