---
title:    "Go: Comparando dos fechas"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Go?

Comparar dos fechas es una operación común en cualquier lenguaje de programación, incluyendo Go. Esto puede ser útil en situaciones como validar fechas de nacimiento, calcular la antigüedad de un evento o simplemente ordenar una lista de fechas en orden cronológico. En este artículo, aprenderemos cómo comparar dos fechas en Go utilizando diferentes métodos y funciones.

## Cómo hacerlo:

Para comparar dos fechas en Go, podemos utilizar el paquete `time` que nos proporciona funciones y métodos útiles para manejar fechas y horas. A continuación, se muestra un ejemplo de cómo comparar dos fechas con la función `Before()`:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Definir dos fechas
	date1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)

	// Comparar si date1 es anterior a date2
	if date1.Before(date2) {
		fmt.Println("La fecha 1 es anterior a la fecha 2")
	} else {
		fmt.Println("La fecha 1 es posterior a la fecha 2")
	}
}
```

El código anterior imprimirá "La fecha 1 es anterior a la fecha 2", ya que 01 de enero es anterior a 01 de febrero.

## Profundizando:

Además de la función `Before()`, también podemos utilizar las funciones `After()` y `Equal()` para comparar dos fechas. Estas funciones devuelven un valor booleano que indica si la primera fecha es posterior, anterior o igual a la segunda fecha, respectivamente. También podemos utilizar el método `Before()` directamente en una variable de tipo `time.Time` para realizar la comparación.

Otra forma de comparar dos fechas es convirtiéndolas en valores numéricos y luego comparando los valores. Podemos utilizar el método `Unix()` para obtener el número de segundos desde la época Unix para una fecha determinada y comparar estos valores entre sí utilizando operadores de comparación.

Por último, debemos tener en cuenta que al comparar fechas, también se debe considerar la zona horaria. Si no se especifica una zona horaria, se asumirá la hora local del sistema.

## Ver también:

- [Documentación oficial de Go sobre el paquete `time`](https://golang.org/pkg/time/)
- [Ejemplos avanzados de comparación de fechas en Go](https://coderwall.com/p/jgmvjw/deep-equality-testing-for-comparable-types-in-go)

¡Ahora que sabes cómo comparar dos fechas en Go, puedes utilizar esta habilidad en tus proyectos y aplicaciones! Recuerda siempre tener en cuenta la zona horaria y utilizar los métodos y funciones de `time` para realizar comparaciones precisas. ¡Happy coding!