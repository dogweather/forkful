---
title:                "Swift: Comparando dos fechas"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

La comparación de dos fechas es una tarea común en programación. Esto nos permite determinar la diferencia entre dos momentos en el tiempo y realizar acciones en función de esa diferencia. En este artículo, aprenderemos cómo comparar dos fechas en Swift.

## Cómo hacerlo

Para comparar dos fechas en Swift, utilizaremos el método `compare()` de la clase `Date`. Este método devuelve un tipo de datos `ComparisonResult`, que puede ser uno de los siguientes valores:

- `orderedAscending` (ordenado ascendente): Indica que la primera fecha es anterior a la segunda.
- `orderedDescending` (ordenado descendente): Indica que la primera fecha es posterior a la segunda.
- `orderedSame` (ordenado igual): Indica que ambas fechas son iguales.

A continuación, se muestra un ejemplo de código que compara dos fechas y muestra el resultado en la consola:

```Swift
let firstDate = Date() // La fecha actual
let secondDate = firstDate.addingTimeInterval(86400) // La fecha de mañana
let comparisonResult = firstDate.compare(secondDate)

switch comparisonResult {
case .orderedAscending:
    print("La primera fecha es anterior a la segunda")
case .orderedDescending:
    print("La primera fecha es posterior a la segunda")
case .orderedSame:
    print("Ambas fechas son iguales")
}
```

La salida de este código sería:

```
La primera fecha es anterior a la segunda
```

También es posible comparar fechas utilizando operadores de comparación, como se muestra en el siguiente ejemplo:

```Swift
let firstDate = Date() // La fecha actual
let secondDate = firstDate.addingTimeInterval(86400) // La fecha de mañana

if firstDate < secondDate {
    print("La primera fecha es anterior a la segunda")
}
```

La salida en este caso sería la misma que en el ejemplo anterior.

## Inmersión profunda

La clase `Date` en Swift también cuenta con otros métodos que nos permiten realizar operaciones con fechas, como por ejemplo `addingTimeInterval()` para sumar o restar un intervalo de tiempo a una fecha. También es posible convertir una fecha a un tipo de datos `String` utilizando un objeto `DateFormatter`.

Es importante tener en cuenta que, al trabajar con fechas, es importante tener en cuenta el huso horario y el formato de fecha que estamos utilizando. Si no especificamos un huso horario, Swift utilizará el huso horario del sistema por defecto.

## Ver también

- [Documentación oficial de Apple sobre la clase `Date`](https://developer.apple.com/documentation/foundation/date)
- [Tutorial sobre cómo trabajar con fechas en Swift](https://www.raywenderlich.com/4781-dates-and-times-in-swift-3-0)
- [GitHub de SwiftDate, una librería de terceros para trabajar con fechas en Swift](https://github.com/malcommac/SwiftDate)