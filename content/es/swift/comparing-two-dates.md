---
title:                "Swift: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar fechas es una tarea común en la programación, ya sea para calcular la duración entre dos eventos o para ordenar una serie de eventos cronológicamente. Aprender cómo comparar fechas en Swift te ayudará a manejar de manera más eficiente tus datos y a realizar operaciones más complejas.

## Cómo hacerlo

Para comparar dos fechas en Swift, primero necesitas crear dos instancias de la clase `Date` con las fechas que quieres comparar. Luego, puedes utilizar el método `compare` para obtener un resultado de tipo `ComparisonResult` que indica si una fecha es anterior, igual o posterior a la otra.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let fechaUno = dateFormatter.date(from: "05/11/2020")
let fechaDos = dateFormatter.date(from: "10/11/2020")

let resultado = fechaUno.compare(fechaDos)
if resultado == .orderedAscending {
    print("La fecha uno es anterior a la fecha dos")
} else if resultado == .orderedSame {
    print("Las fechas son iguales")
} else {
    print("La fecha uno es posterior a la fecha dos")
}
```

La salida del código anterior sería "La fecha uno es anterior a la fecha dos", ya que 5 de noviembre es anterior a 10 de noviembre.

Si quieres comparar con una precisión mayor a los días, también puedes utilizar el método `compare(_:toGranularity:fetch:)` y especificar la granularidad deseada (como hora, minuto, segundo, etc.).

## Profundizando

Para comparar fechas de manera más compleja, Swift ofrece la estructura `Calendar` y los métodos `dateComponents(_:from:to:)` y `compare(_:with:toGranularity:)`. Esto te permite obtener no solo la diferencia entre dos fechas, sino también la cantidad de unidades de tiempo (días, horas, minutos, etc.) entre ellas.

Además, también es importante tener en cuenta que las fechas pueden variar dependiendo de la zona horaria en la que se encuentren, así que es recomendable utilizar la clase `TimeZone` para realizar comparaciones más precisas.

## Ver también

- [Documentación oficial de Apple sobre comparar fechas en Swift](https://developer.apple.com/documentation/foundation/date/performing_calculations_with_dates)
- [Tutorial de comparación de fechas en Swift](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates)
- [Explicación detallada sobre cómo comparar fechas en diferentes zonas horarias](https://www.swiftbysundell.com/articles/comparing-dates-in-different-time-zones-in-swift/)