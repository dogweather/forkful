---
title:                "Go: Comparando dos fechas"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué comparar dos fechas en Go

La comparación de fechas es una tarea común en muchas aplicaciones de programación, incluyendo aquellas escritas en Go. Al comparar dos fechas, podemos determinar si una es anterior, posterior o igual a la otra, lo que puede ser útil para ordenar eventos, realizar cálculos de tiempo, o validar entradas de usuarios. En esta publicación, aprenderemos cómo comparar dos fechas en Go y exploraremos algunos aspectos más profundos de esta operación.

## Cómo hacerlo

En Go, podemos comparar dos fechas utilizando el operador de comparación "==" o los métodos `Equal()` y `Before()` de la librería `time`. Veamos un ejemplo:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Creamos dos fechas utilizando el método `Date` de la librería `time`
	date1 := time.Date(2021, time.October, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.October, 10, 0, 0, 0, 0, time.UTC)

	// Comparamos las dos fechas utilizando el operador "==" y los métodos
	// `Equal()` y `Before()` que retornan un valor booleano
	fmt.Println(date1 == date2)        // false
	fmt.Println(date1.Equal(date2))    // false
	fmt.Println(date1.Before(date2))   // true
	fmt.Println(date1.After(date2))    // false
}
```

En este ejemplo, creamos dos fechas distintas y las comparamos utilizando diferentes métodos. Nota que el método `After()` también retorna un valor booleano, indicando si la primera fecha es posterior a la segunda.

## Inmersión profunda

Cuando comparamos dos fechas en Go, es importante tener en cuenta que estamos analizando tanto la fecha como la hora. Esto significa que incluso si dos fechas tienen la misma fecha, pueden ser consideradas distintas si sus horas son diferentes. Por ejemplo:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date1 := time.Date(2021, time.October, 1, 12, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.October, 1, 18, 0, 0, 0, time.UTC)

	// Comparamos las fechas, pero solo nos fijamos en la fecha usando el método `Truncate`
	fmt.Println(date1.Truncate(24*time.Hour) == date2.Truncate(24*time.Hour)) // true
	fmt.Println(date1.Equal(date2))                                           // false
}
```

En este ejemplo, estamos ignorando la hora al utilizar el método `Truncate()` y podemos ver que las dos fechas son iguales.

Es importante tener en cuenta estas consideraciones al comparar fechas en Go, dependiendo de nuestras necesidades específicas.

## Ver también

- Documentación oficial de la librería `time` en Go: https://golang.org/pkg/time/
- Ejemplos de comparación de fechas en Go: https://www.calhoun.io/comparing-dates-in-go/

_Si quieres profundizar más en el tema de comparación de fechas en Go, revisa estos recursos adicionales._