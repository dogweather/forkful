---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Go: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado calcular una fecha en el futuro o en el pasado? Esto puede ser útil para programar eventos, automatizar tareas o simplemente por curiosidad. Aprender a hacerlo en Go puede mejorar tus habilidades de programación y hacer tu vida un poco más fácil.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, necesitamos utilizar la librería `time` de Go. Primero, debemos importarla en nuestro código:

```Go
import "time"
```

Luego, podemos utilizar la función `Now()` para obtener la fecha y hora actual y almacenarla en una variable:

```Go
ahora := time.Now()
```

A partir de aquí, podemos utilizar esta variable para calcular una fecha en el futuro o en el pasado. Por ejemplo, si queremos obtener la fecha de mañana, podemos añadir un día a la fecha actual utilizando la función `AddDate()` y almacenar el resultado en una nueva variable:

```Go
manana := ahora.AddDate(0, 0, 1)
```

La función `AddDate()` toma tres argumentos: el número de años, meses y días que queremos añadir a la fecha actual. En este caso, hemos añadido 0 años, 0 meses y 1 día.

También podemos utilizar la función `Sub()` para restar una cantidad de tiempo a la fecha actual. Por ejemplo, si queremos obtener la fecha de hace una semana, podemos restarle 7 días a la fecha actual y almacenar el resultado en una nueva variable:

```Go
haceUnaSemana := ahora.Sub(time.Hour * 24 * 7)
```

La función `Sub()` toma como argumento un `Duration`, que en este caso hemos creado multiplicando un día (24 horas) por 7.

## Buceo profundo

La librería `time` de Go ofrece muchas funciones y métodos útiles para trabajar con fechas y horas. Además de `Now()`, `AddDate()` y `Sub()`, también podemos utilizar las funciones `Date()` y `Parse()` para crear fechas específicas a partir de valores numéricos o cadenas de texto. También podemos trabajar con zonas horarias y realizar operaciones más complejas como comparar fechas o calcular la diferencia entre dos fechas.

Con un poco de práctica, puedes utilizar estas funciones para realizar cálculos precisos y útiles con fechas en tus proyectos de Go.

## Ver también

- Documentación de la librería `time` de Go: https://golang.org/pkg/time/
- Ejemplos de cálculos de fechas en Go: https://golangbyexample.com/golang-time-series-datetime/
- Tutorial sobre cómo trabajar con fechas y horas en Go: https://www.calhoun.io/working-with-dates-and-times-in-go/