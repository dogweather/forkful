---
title:                "Comparando dos fechas"
html_title:           "Go: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo comparar dos fechas en un programa de Go? ¡Este artículo es para ti! Aprenderás cómo hacerlo de manera sencilla y eficiente en este lenguaje de programación.

## Cómo

Para comparar dos fechas en Go, primero debemos tener en cuenta que necesitamos trabajar con el tipo de dato `time.Time`. Este tipo nos permite almacenar información sobre la fecha y la hora.

Para crear dos fechas que queramos comparar, podemos utilizar la función `time.Parse`, que recibe como parámetros un formato y una cadena de texto que representa la fecha. Por ejemplo:

```Go
// Creamos dos fechas utilizando la función time.Parse
fecha1, _ := time.Parse("2006-01-02", "2019-12-25")
fecha2, _ := time.Parse("2006-01-02", "2020-01-01")

// Comparamos las fechas utilizando el operador ==
if fecha1 == fecha2 {
  fmt.Println("Las fechas son iguales")
} else {
  fmt.Println("Las fechas son diferentes")
}

// Utilizamos los métodos After y Before para comparar si una fecha es anterior o posterior a otra
if fecha1.After(fecha2) {
  fmt.Println("La fecha 1 es posterior a la fecha 2")
} else if fecha1.Before(fecha2) {
  fmt.Println("La fecha 1 es anterior a la fecha 2")
} else {
  fmt.Println("Las fechas son iguales")
}
```

En el primer ejemplo, utilizamos el operador `==` para comparar dos fechas y en el segundo ejemplo, utilizamos los métodos `After` y `Before` para comparar si una fecha es anterior o posterior a la otra. Como resultado, obtendremos lo siguiente:

```
Las fechas son diferentes
La fecha 1 es anterior a la fecha 2
```

## Deep Dive

Go ofrece una amplia gama de métodos para trabajar con fechas y realizar comparaciones. Algunos de estos métodos son:

- `Date()` que devuelve la fecha en formato `año-mes-día`.
- `Year()` que devuelve el año de la fecha.
- `Month()` que devuelve el mes de la fecha.
- `Day()` que devuelve el día de la fecha.
- `Before()` y `After()` permiten comparar fechas y devuelven un booleano indicando si una fecha es anterior o posterior a la otra.

Para obtener más información sobre estos y otros métodos, puedes consultar la documentación oficial de Go.

## Ver también

- [Documentación oficial de Go](https://golang.org/doc/)
- [Tutorial de Go en español](https://www.tutorialesprogramacionya.com/goya/index.php?inicio=0)