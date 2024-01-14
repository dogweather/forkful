---
title:    "Go: Calculando una fecha en el futuro o pasado"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué
Alguna vez te has preguntado cómo calcular una fecha en el futuro o en el pasado en un programa? En este blog post, te explicaremos por qué esta funcionalidad puede ser útil y cómo puedes implementarla en tus proyectos de Go.

## Cómo Hacerlo
Para calcular una fecha en el futuro o en el pasado en Go, podemos utilizar la función `Time.Add()` junto con el tipo de dato `Duration`. Por ejemplo, si queremos calcular la fecha de mañana, podemos hacer lo siguiente:

```Go
// Importamos el paquete de tiempo
import "time"

// Creamos una variable con la fecha de hoy
hoy := time.Now()

// Calculamos la fecha de mañana
manana := hoy.Add(24 * time.Hour) 

// Imprimimos el resultado en la consola
fmt.Println(manana)
```

Este código nos devuelve la fecha de mañana en el mismo formato que la fecha de hoy. Podemos también calcular una fecha en el pasado utilizando un número negativo para la `Duration`, por ejemplo:

```Go
ayer := hoy.Add(-24 * time.Hour)
```

De esta manera, podemos obtener la fecha de ayer. También podemos utilizar otras unidades de tiempo como `Minute`, `Second`, `Millisecond`, etc. para calcular fechas más específicas.

## Inmersión Profunda
La función `Time.Add()` funciona agregando o restando un intervalo de tiempo al tiempo actual. Sin embargo, también podemos calcular una fecha basada en una fecha específica utilizando la función `Time.Date()` y luego agregando la `Duration`. Por ejemplo:

```Go
// Calcula la fecha dentro de 2 meses y 3 días
fecha := time.Date(2021, time.March, 20, 0, 0, 0, 0, time.UTC)
futuro := fecha.Add(2 * time.Month + 3 * time.Day)
```

De esta manera, podemos personalizar aún más nuestras fechas y obtener resultados más precisos.

## Ver También
- [Paquete de Tiempo en la documentación oficial de Go](https://golang.org/pkg/time/)
- [Tutorial de Tiempo en Go](https://gobyexample.com/time)
- [Calculando fechas con Go](https://golangexample.com/calculate-dates-in-the-golang-example/#:~:text=Calculating%20Dates%20in%20the%20Golang%20Example)