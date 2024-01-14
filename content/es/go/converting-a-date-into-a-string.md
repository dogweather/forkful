---
title:    "Go: Convirtiendo una fecha en una cadena."
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué
Convertir un objeto de fecha a cadena de caracteres es una tarea común en la programación Go. Muchas veces, necesitamos mostrar una fecha en un formato específico o guardarla como una cadena en una base de datos. Afortunadamente, Go nos proporciona una forma sencilla de realizar esta conversión.

## Cómo hacerlo
Para convertir una fecha a cadena de caracteres en Go, primero debemos tener un objeto de tipo `time.Time`. Luego, utilizaremos el método `Format` para especificar el formato de salida deseado. Por ejemplo:

```
// Crear un objeto de fecha
fecha := time.Date(2021, 3, 14, 12, 30, 0, 0, time.UTC)

// Convertirlo a una cadena en formato ISO 8601
cadena := fecha.Format("2006-01-02T15:04:05Z")

fmt.Println(cadena) //Salida: 2021-03-14T12:30:00Z
```

En el código anterior, utilizamos la función `Date` para crear un objeto de fecha con los atributos proporcionados. Luego, usamos el método `Format` para especificar el formato en el cual queremos mostrar la fecha. En este caso, utilizamos el estándar ISO 8601 que sigue el formato `AAAA-MM-DDThh:mm:ssZ`.

También podemos incluir otros elementos, como el nombre del mes o el día de la semana, utilizando los códigos de formato correspondientes. Por ejemplo:

```
// Convertir la fecha en una cadena con el nombre del mes completo y el día de la semana abreviado
cadena := fecha.Format("2 de January, 2006 (Mon)")

fmt.Println(cadena) //Salida: 14 de marzo, 2021 (Sun)
```

## Profundizando
En Go, la conversión de una fecha a cadena de caracteres utiliza el paquete `time` que proporciona una gran flexibilidad en cuanto a formatos de salida. Además, podemos utilizar la función `Parse` para convertir una cadena a un objeto de fecha en formato `time.Time`.

También es importante tener en cuenta que, al convertir una fecha a cadena, se toma en cuenta la zona horaria por defecto del sistema. Sin embargo, podemos especificar una zona horaria diferente utilizando el método `In`, lo que resulta útil en aplicaciones multi-zona horaria.

## Ver también
- Documentación oficial de `time` en Go: https://golang.org/pkg/time/
- Ejemplos de formato de fecha y hora en Go: https://programming.guide/go/format-parse-string-time-date-example.html
- Tutorial de Go en español: https://www.tutorialesprogramacionya.com/goya/index.php