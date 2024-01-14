---
title:    "Swift: Obteniendo la fecha actual"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual puede parecer una tarea simple, pero es una habilidad esencial en la programación Swift. Puede ser útil para muchas aplicaciones, como mostrar la fecha actual en una aplicación de calendario o registrar el tiempo de una transacción en una aplicación de banca en línea.

## Cómo hacerlo

Para obtener la fecha actual en Swift, utilizaremos la clase `Date`. Esta clase contiene métodos y propiedades para trabajar con fechas y horas.

```Swift
let currentDate = Date() // Obtenemos la fecha actual
print(currentDate) // Imprimimos la fecha en la consola
```

La salida en la consola será algo similar a esto:

`2019-09-10 18:23:55 +0000`

Puedes notar que la fecha y hora están en formato de UTC (Tiempo Universal Coordinado). Para obtener la fecha en un formato más legible, podemos utilizar un `DateFormatter`.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/YYYY" // Establecemos el formato deseado
let formattedDate = dateFormatter.string(from: currentDate) // Convertimos la fecha a un String en el formato deseado
print(formattedDate) // Imprimimos la fecha en la consola
```

La salida sería algo como esto:

`10/09/2019`

Existen diferentes formatos de fechas que puedes utilizar con `dateFormat`, por ejemplo `dd/MM/YY`, `dd MMMM yyyy`, entre otros. Puedes experimentar con diferentes formatos para encontrar el que mejor se adapte a tus necesidades.

## Deep Dive

Además de obtener la fecha actual, también podemos realizar operaciones como comparar fechas y obtener la diferencia entre ellas. Podemos utilizar el método `compare` de `Date` para comparar dos fechas y obtener un `ComparisonResult`.

```Swift
let date1 = Date() // Fecha actual
let date2 = Date().addingTimeInterval(86400) // Fecha actual + 1 día

let result = date1.compare(date2) // Comparamos las dos fechas

switch result {
case .orderedAscending:
    print("date1 es menor que date2")
case .orderedDescending:
    print("date1 es mayor que date2")
case .orderedSame:
    print("Las fechas son iguales")
}
```

La salida sería "date1 es menor que date2", ya que hemos agregado 1 día a la segunda fecha.

Podemos obtener la diferencia entre dos fechas utilizando el método `timeIntervalSince` de `Date`. Este método nos devuelve la diferencia en segundos entre ambas fechas.

```Swift
let difference = date2.timeIntervalSince(date1) // Obtenemos la diferencia en segundos
print(difference) // Imprimimos la diferencia en la consola
```

La salida sería "86400", que es la cantidad de segundos en un día.

## Ver también

- [Documentación de la clase Date](https://developer.apple.com/documentation/foundation/date)
- [Guía de formato de fecha en Swift](https://www.appcoda.com/swift-date-formatter/)
- [Tutoriales de Swift en español](https://www.aulaformativa.com/l/swift/)