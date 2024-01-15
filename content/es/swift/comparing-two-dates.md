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

## ¿Por qué?

Comparar fechas es una tarea común en la programación, especialmente cuando se trabaja con aplicaciones que manejan eventos o tareas que ocurren en momentos específicos. Aunque puede parecer simple, es importante entender cómo comparar fechas correctamente para evitar errores en nuestro código.

## Cómo

Para comparar dos fechas en Swift, podemos utilizar el método `compare` de la clase `Date`. Este método compara dos fechas y devuelve un `ComparisonResult`, que puede ser `orderedAscending`, `orderedDescending` o `orderedSame` dependiendo de la relación entre las dos fechas.

```Swift
let dateA = Date()
let dateB = Date()

if dateA.compare(dateB) == .orderedAscending {
    print("\(dateA) es anterior a \(dateB)")
}
```

En este ejemplo, utilizamos el operador `==` para comparar el resultado del método `compare` con `.orderedAscending`, indicando que la `dateA` es anterior a `dateB`. También podemos utilizar operadores de comparación como `<` y `>` para verificar la relación entre dos fechas.

También podemos comparar fechas con una precisión mayor utilizando la clase `Calendar` y su método `isDate(_:equalToDate:toGranularity:)`. Este método acepta un segundo parámetro que nos permite especificar la granularidad de la comparación, como año, mes, día, hora, etc.

```Swift
let calendar = Calendar.current
let date1 = calendar.date(from: DateComponents(year: 2020, month: 1, day: 1, hour: 12))
let date2 = calendar.date(from: DateComponents(year: 2020, month: 1, day: 1, hour: 15))

if calendar.isDate(date1, equalTo: date2, toGranularity: .hour) {
    print("\(date1) y \(date2) tienen la misma hora")
}
```

## Deep Dive

Es importante tener en cuenta que comparar fechas con una precisión mayor puede ser un poco más complejo. Esto se debe a que también debemos considerar factores como la zona horaria y el calendario utilizado. Si no especificamos estos datos, podríamos obtener resultados inesperados en nuestra comparación.

Otra consideración importante es que si estamos trabajando con fechas almacenadas en una base de datos, es posible que debamos convertirlos en objetos `Date` antes de realizar una comparación. Para esto, podemos utilizar un `DateFormatter` para parsear la cadena de fecha de la base de datos y crear un objeto `Date`.

## Ver también

Para más información sobre cómo trabajar con fechas en Swift, puedes consultar la documentación oficial de Apple sobre las clases `Date` y `Calendar`, así como sobre cómo utilizar un `DateFormatter`: 
- https://developer.apple.com/documentation/foundation/date
- https://developer.apple.com/documentation/foundation/calendar
- https://developer.apple.com/documentation/foundation/dateformatter