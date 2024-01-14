---
title:    "Go: Convirtiendo una fecha en una cadena"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Convertir una fecha en una cadena es una tarea común en muchos proyectos de programación. Puede ser útil para mostrar fechas de una manera legible para los usuarios o para realizar operaciones matemáticas con fechas. En Go, hay varias formas de lograr esto, y en este artículo exploraremos algunas de ellas.

## Cómo hacerlo

Para convertir una fecha en una cadena en Go, podemos usar la función `Format` de la biblioteca `time`. Por ejemplo, si queremos mostrar la fecha actual en formato "mes/día/año", podemos usar el siguiente código:

```Go
fechaActual := time.Now()
cadenaFecha := fechaActual.Format("01/02/2006")
fmt.Println("Hoy es", cadenaFecha) // Salida: Hoy es 11/05/2021
```

Podemos personalizar el formato cambiando el parámetro en la función `Format`. Por ejemplo, si queremos mostrar la fecha con la hora y minutos, podemos usar "01/02/2006 15:04":

```Go
fechaActual := time.Now()
cadenaFechaHora := fechaActual.Format("01/02/2006 15:04")
fmt.Println("La hora actual es", cadenaFechaHora) // Salida: La hora actual es 11/05/2021 14:30
```

Incluso podemos convertir una fecha en una cadena con un formato internacional, como "dd/mm/aaaa":

```Go
fechaActual := time.Now()
cadenaFechaInternacional := fechaActual.Format("02/01/2006")
fmt.Println("Hoy es", cadenaFechaInternacional) // Salida: Hoy es 05/11/2021
```

## Profundizando

La función `Format` de `time` nos permite generar una cadena a partir de una fecha utilizando un patrón específico de formato. Algunos de los patrones más comunes son:

- 01 : representación numérica del mes (01 para enero, 12 para diciembre)
- 02 : representación numérica del día (01 para el primer día del mes, 31 para el último)
- 2006 : representación numérica del año (ej. 2021)
- 15 : representación numérica de las horas en formato de 24 horas
- 04 : representación numérica de los minutos
- 05 : representación numérica de los segundos

Puede consultar la documentación de Go para ver la lista completa de patrones y utilizarlos en la función `Format` según sea necesario.

## Véase también

- [Documentación de Go sobre la biblioteca `time`](https://golang.org/pkg/time/)
- [Blog sobre formateo de fechas en Go](https://www.calhoun.io/how-to-format-string-dates-in-go/)
- [Ejemplos de formateo de fechas en Go](https://yourbasic.org/golang/format-parse-string-time-date-example/)