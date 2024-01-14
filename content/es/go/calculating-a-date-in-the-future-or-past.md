---
title:                "Go: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas en el futuro o en el pasado?

Calcular fechas en el futuro o en el pasado puede ser útil en muchos casos, como por ejemplo en la programación de tareas o eventos, en el manejo de fechas de caducidad, o simplemente para realizar cálculos en un programa de forma más eficiente.

## Cómo hacerlo en Go

Para calcular una fecha en el futuro o en el pasado en Go, podemos utilizar la función `date.AddDate()` de la librería `time`. Por ejemplo, si queremos calcular el día de hoy más 7 días en el futuro, podemos hacerlo de la siguiente manera:

```Go
import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("Hoy es:", time.Now()) // imprimimos la fecha de hoy
	fechaFutura := time.Now().AddDate(0, 0, 7) // usamos la función AddDate para sumarle 7 días a la fecha de hoy
	fmt.Println("Dentro de 7 días será:", fechaFutura) // imprimimos la fecha resultante
}
```

El resultado de este código sería:

```
Hoy es: 2021-08-01 13:22:02.123456789 +0000 UTC m=+0.000000001
Dentro de 7 días será: 2021-08-08 13:22:02.123456789 +0000 UTC m=+604800.000000001
```

Podemos jugar con los valores que le pasamos a la función `AddDate()` para sumar o restar años, meses y días en el cálculo de la fecha.

## Profundizando en el cálculo de fechas en Go

En Go también podemos utilizar la función `time.Parse()` para convertir una cadena de texto con una fecha en un objeto de tipo `time.Time`, el cual luego podemos utilizar para hacer cálculos de fechas. Por ejemplo, si queremos calcular cuántos días faltan para mi próximo cumpleaños, podemos hacer lo siguiente:

```Go
import (
	"fmt"
	"time"
)

func main() {
	hoy := time.Now()
	cumple := "2021-12-01" // fecha de mi cumpleaños en formato año-mes-día
	fechaCumple, _ := time.Parse("2006-01-02", cumple) // convertimos la cadena de texto a un objeto de tipo time.Time
	diasFaltantes := fechaCumple.Sub(hoy).Hours() / 24 // utilizamos la función Sub para calcular la diferencia de días entre ambas fechas
	fmt.Printf("Faltan %v días para mi cumpleaños\n", int(diasFaltantes)) // imprimimos el resultado
}
```

El resultado sería algo así:

```
Faltan 120 días para mi cumpleaños
```

## Ver también

- Documentación de la función `AddDate()` en la librería `time`: https://golang.org/pkg/time/#Time.AddDate
- Documentación de la función `Parse()` en la librería `time`: https://golang.org/pkg/time/#Parse
- Ejemplos de cálculo de fechas con diferentes formatos en Go: https://gobyexample.com/time-formatting-parsing