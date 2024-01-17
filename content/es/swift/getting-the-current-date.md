---
title:                "Obteniendo la fecha actual"
html_title:           "Swift: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

Qué es y por qué lo hacemos:

Obtener la fecha actual es una tarea común que los programadores realizan para obtener información sobre el momento en el que se está ejecutando un programa. Esto puede ser útil para realizar operaciones basadas en fechas, como mostrar la fecha actual en una aplicación o calcular la duración entre dos fechas.

Cómo hacerlo:

Para obtener la fecha actual en Swift, podemos utilizar el objeto `Date()` y el objeto `DateFormatter()`. Primero, creamos un objeto `Date()` para representar la fecha actual y luego lo formateamos utilizando un objeto `DateFormatter()`. Luego podemos imprimir la fecha formateada utilizando el método `string(from: Date)`.

```Swift
let fechaActual = Date()
let formatoFecha = DateFormatter()
formatoFecha.dateFormat = "dd-MM-yyyy"
let fechaFormateada = formatoFecha.string(from: fechaActual)
print(fechaFormateada) // Salida: 18-03-2021
```

Deep Dive:

La necesidad de obtener la fecha actual ha existido desde los primeros días de la programación. Antes del año 1970, los sistemas de tiempo se basaban en el reloj del sistema y no tenían una forma estándar de representar la fecha y hora. Sin embargo, con el desarrollo de sistemas operativos modernos, se ha establecido una forma estándar de representar la fecha y hora utilizando el llamado "Tiempo Unix", que es la cantidad de segundos transcurridos desde el 1 de enero de 1970.

Hay varias alternativas para obtener la fecha actual en Swift, como utilizar bibliotecas externas o la API de tiempo de iOS, pero la forma presentada anteriormente es una solución simple y nativa que debería ser suficiente para la mayoría de los casos.

Más información:

Para obtener más información sobre cómo trabajar con fechas en Swift, puedes visitar la documentación oficial de Apple: https://developer.apple.com/documentation/foundation/date.

También puedes explorar otras formas de obtener la fecha actual en Swift y sus ventajas y desventajas en este artículo: https://swiftwithmajid.com/2020/01/15/getting-current-date-and-time-in-swift/.

¡Happy coding!