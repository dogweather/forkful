---
title:    "Swift: Comparando dos fechas"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué

Comparar fechas es una parte esencial de la programación en Swift. Puede ser útil para determinar si una fecha es anterior, posterior o igual a otra. Esta habilidad es especialmente importante en aplicaciones que manejan calendarios, horarios o recordatorios.

## Cómo hacerlo

La comparación de fechas en Swift es sencilla y se puede hacer de diversas maneras. Una forma de hacerlo es utilizando el operador de comparación ```<```, ```>``` o ```==```. Por ejemplo, si queremos comparar dos fechas y ver si la primera es anterior a la segunda, podemos escribir:

```
let fecha1 = Date()
let fecha2 = Date(timeIntervalSinceNow: 86400) // fecha2 es un día después de fecha1

if fecha1 < fecha2 {
  print("fecha1 es anterior a fecha2")
}
```

En este ejemplo, utilizamos la función ```Date()``` para obtener la fecha actual y la función ```Date(timeIntervalSinceNow:)``` para obtener una fecha un día después. Después, utilizamos el operador de comparación ```<``` para comparar ambas fechas y mostrar un mensaje en caso de que la primera sea anterior a la segunda.

Otra forma de comparar fechas es utilizando el método ```compare(_:)``` de la clase ```NSDate```, que devuelve un valor del tipo ```ComparisonResult``` que puede ser ```orderedAscending```, ```orderedDescending``` o ```orderedSame```. Por ejemplo:

```
let fecha1 = Date()
let fecha2 = Date(timeIntervalSinceNow: 86400)

let resultado = fecha1.compare(fecha2)

switch resultado {
case .orderedAscending:
  print("fecha1 es anterior a fecha2")
case .orderedDescending:
  print("fecha1 es posterior a fecha2")
case .orderedSame:
  print("fecha1 es igual a fecha2")
}
```
En este caso, utilizamos el método ```compare(_:)``` para obtener el resultado de la comparación y luego utilizamos un switch para mostrar un mensaje dependiendo del resultado.

## Profundizando

La comparación de fechas puede ser más compleja si se tienen en cuenta diferentes zonas horarias, formatos de fecha y localizaciones. En esos casos, es importante utilizar formateadores de fecha como ```DateFormatter``` para garantizar que las fechas se comparen correctamente.

También es importante tener en cuenta la precisión de las fechas, ya que si estamos trabajando con horas, minutos y segundos, una pequeña diferencia en alguno de ellos puede afectar el resultado de la comparación.

## Ver también

- Documentación oficial de Swift sobre el manejo de fechas y horas (https://docs.swift.org/swift-book/LanguageGuide/DatesAndTimes.html)
- Tutorial de Ray Wenderlich sobre cómo trabajar con fechas en Swift (https://www.raywenderlich.com/767-swift-4-0-cheat-sheet-and-quick-reference)
- Tutorial de AppCoda sobre cómo utilizar formateadores de fecha en Swift (https://www.appcoda.com/nsdateformatter/)