---
title:    "Swift: Convirtiendo una fecha en una cadena"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena?

Al trabajar con fechas en Swift, a menudo surge la necesidad de convertir una fecha en una cadena legible para los usuarios. Esto puede ser necesario para mostrar la fecha en un formato específico, almacenarla en una base de datos o para otros fines. Afortunadamente, convertir una fecha en una cadena es un proceso sencillo y este artículo te mostrará cómo hacerlo.

## Cómo hacerlo

Para convertir una fecha en una cadena, utilizaremos el tipo de dato `DateFormatter` de Swift. Este tipo nos permite dar formato a una fecha según nuestras necesidades. Veamos un ejemplo:

```Swift
let fecha = Date() // obtenemos la fecha actual
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy" // formato que queremos
let dateString = formatter.string(from: fecha) // convertimos la fecha en una cadena
print(dateString) // salida: 24/10/2021
```

En este ejemplo, creamos un objeto `DateFormatter` y le asignamos un formato específico utilizando la propiedad `dateFormat`. Luego, usamos el método `string(from: fecha)` para convertir la fecha en una cadena según el formato que especificamos. Por último, imprimimos la cadena resultante en la consola.

## Profundizando

El tipo de dato `DateFormatter` ofrece muchas opciones para formatear una fecha. Además del formato utilizado en el ejemplo anterior, también puedes usar otros formatos como "MMM d, yyyy" para obtener una cadena como "Oct 24, 2021". También puedes personalizar el formato a tu gusto utilizando patrones especiales de formato. Puedes encontrar una lista completa de estos patrones [aquí](https://nsdateformatter.com).

Además, el tipo `DateFormatter` también permite especificar la zona horaria, el idioma y otras opciones avanzadas para dar formato a la fecha.

## Ver también

- [Cómo trabajar con fechas en Swift](https://www.example.com/fechas-en-swift)
- [Cómo convertir una cadena en una fecha en Swift](https://www.example.com/conversion-cadena-fecha-swift)
- [Documentación oficial de DateFormatter en Swift](https://developer.apple.com/documentation/foundation/dateformatter)