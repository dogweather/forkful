---
title:                "Go: Convirtiendo una fecha en una cadena"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Convertir una fecha en una cadena de texto puede ser una tarea común en la programación. Puede ser necesario mostrar una fecha en un formato específico para una interfaz de usuario o para almacenarla en una base de datos. En este artículo aprenderemos cómo hacerlo utilizando el lenguaje de programación Go.

## Cómo hacerlo
Para convertir una fecha en una cadena de texto en Go, debemos utilizar la función `Format` del paquete `time`. Primero, debemos crear un objeto `time` con la fecha que queremos convertir. Luego, utilizando un formato específico, podemos convertir la fecha en una cadena de texto.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Crear un objeto time con una fecha específica
    date := time.Date(2019, 6, 28, 0, 0, 0, 0, time.UTC)

    // Convertir la fecha en una cadena de texto utilizando el formato "MM/dd/yyyy"
    dateString := date.Format("01/02/2006")

    // Imprimir la cadena de texto
    fmt.Println(dateString) // Output: 06/28/2019
}
```

En este ejemplo, utilizamos el formato "01/02/2006" ya que es el diseño de fecha utilizado en el lenguaje Go. Este formato se basa en el patrón de fecha americana, donde 01 representa el mes, 02 el día y 2006 el año.

## Profundizando
Además del formato "MM/dd/yyyy", podemos utilizar una variedad de diseños de fecha para convertir una fecha en una cadena de texto. Algunos ejemplos son:

- "Jan 2, 2006" - Muestra la fecha en formato Jan 02, 2006 (por ejemplo, Jun 28, 2019)
- "Monday, Jan 2, 2006" - Muestra la fecha en formato Monday, Jan 02, 2006 (por ejemplo, Friday, Jun 28, 2019)
- "January 2, 2006" - Muestra la fecha en formato January 02, 2006 (por ejemplo, June 28, 2019)

También podemos utilizar una combinación de letras y símbolos para agregar información adicional en la cadena de texto final, como mostrar el día de la semana o el horario. Es importante tener en cuenta que siempre debemos utilizar el número correcto de letras para representar cada elemento de la fecha, de lo contrario obtendremos una salida incorrecta.

## Ver también
- [Documentación oficial del paquete `time` en Go](https://golang.org/pkg/time/)
- [Preguntas frecuentes sobre el formato de fecha en Go](https://stackoverflow.com/questions/35428209/what-does-jan-2-2006-mean-in-golangs-timestring)
- [Tutorial de Go para principiantes](https://golangbot.com/learn-golang-series/) (en español)