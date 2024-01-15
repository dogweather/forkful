---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Go: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Si eres un programador de Go, es probable que en algún momento necesites convertir una fecha en formato de cadena para mostrarla en tu programa. Puede ser para mostrar la fecha en un formato personalizado o para almacenarla en una base de datos. Afortunadamente, Go tiene una manera fácil de convertir fechas en cadenas.

## Cómo hacerlo

Para convertir una fecha en una cadena en Go, simplemente utilizamos la función `Format()` del paquete `time`. Aquí hay un ejemplo de cómo se vería esto en código:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Creamos una variable de tipo time
    date := time.Date(2021, time.January, 15, 0, 0, 0, 0, time.Local)
    // Utilizamos la función Format() para convertir en una cadena con el formato deseado
    stringDate := date.Format("02/01/2006")
    // Imprimimos la cadena resultante
    fmt.Println(stringDate)
}
```

La cadena de formato que utilizamos en este ejemplo (`02/01/2006`) sigue el estándar de la fecha de Unix. Podemos cambiarla según nuestras necesidades, por ejemplo, si queremos incluir el día de la semana, podemos agregar `Monday` a la cadena de formato.

La salida del código anterior sería:

`15/01/2021`

## Profundizando

Ahora que sabemos cómo convertir una fecha en una cadena, podríamos preguntarnos: ¿cómo funciona realmente esta conversión? En realidad, el paquete `time` de Go utiliza un tipo interno llamado `time.Time` para representar fechas y horas. Este tipo tiene un método llamado `Format()`, que recibe una cadena de formato y devuelve una cadena que representa la fecha en el formato deseado.

Otro aspecto importante a tener en cuenta es el parámetro de zona horaria. En nuestro ejemplo, utilizamos el parámetro `time.Local` para indicar que queremos utilizar la hora local del sistema. Sin embargo, también podemos especificar la zona horaria que deseamos, por ejemplo `time.UTC` para la hora universal coordinada o `time.FixedZone()` para una zona horaria específica.

Conocer estos detalles internos nos puede proporcionar un mayor control y comprensión sobre cómo se manejan las fechas en Go.

## Ver también

- [Documentación oficial de la función `Format()` en el sitio web de Go](https://golang.org/pkg/time/#Time.Format)
- [Tutorial de fechas y horas en Go en el sitio web de Learn Go](https://www.learn-golang.org/docs/time/)
- [Artículo sobre cómo trabajar con fechas en Go en el blog de Gopher Academy](https://blog.gopheracademy.com/advent-2017/work-with-dates-like-a-pro/)