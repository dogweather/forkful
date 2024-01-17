---
title:                "Comparando dos fechas"
html_title:           "Swift: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Comparar dos fechas es una tarea común en la programación, que consiste en determinar si una fecha es anterior, posterior o igual a otra fecha. Los programadores realizan esta acción para ordenar eventos, realizar cálculos de tiempo e implementar lógica en sus aplicaciones.

## Cómo:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"

let date1 = dateFormatter.date(from: "01/01/2020")
let date2 = dateFormatter.date(from: "25/12/2020")

if date1! < date2! {
    print("La fecha 1 es anterior a la fecha 2")
} else if date1! > date2! {
    print("La fecha 1 es posterior a la fecha 2")
} else {
    print("Las fechas son iguales")
}
```

Salida:

```
La fecha 1 es anterior a la fecha 2
```

## En Profundidad:

Al comparar dos fechas en Swift, se deben tener en cuenta varios factores, como el formato en el que se encuentran las fechas y la precisión. Además, existen varias formas de realizar la comparación, como utilizando los operadores <, > y == o utilizando métodos específicos como `compare()`.

Es importante mencionar que en Swift, las fechas se manejan como objetos del tipo `Date`, que representan un punto en el tiempo en lugar de un formato específico como en otros lenguajes.

## Ver También:

- [Documentación oficial de Swift sobre fechas](https://docs.swift.org/swift-book/LanguageGuide/DatesAndTimes.html)
- [Tutorial sobre comparación de fechas en Swift](https://www.appcoda.com/swift-date-time/)