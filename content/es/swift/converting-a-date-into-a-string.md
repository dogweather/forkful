---
title:    "Swift: Convirtiendo una fecha en una cadena"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado cómo mostrar una fecha en tu aplicación de Swift? Convertir una fecha en una cadena de texto puede ser una tarea útil en muchas situaciones, como mostrar la fecha actual en pantalla o guardarla en una base de datos. En esta publicación del blog, te mostraremos cómo hacerlo de manera sencilla en Swift.

## Cómo hacerlo
¡Es muy fácil convertir una fecha en una cadena de texto en Swift! Solo necesitas seguir estos sencillos pasos:

1. Primero, crea una variable de tipo Date con la fecha que deseas convertir.
2. Luego, declara un formato de fecha usando DateFormatter. Puedes elegir entre diferentes formatos predefinidos o crear uno personalizado.
3. Después, usa el método `string(from:)` de DateFormatter para convertir la fecha en una cadena de texto.
4. Finalmente, asigna la cadena resultante a una variable o imprímela en la consola.

Aquí te dejamos un ejemplo de código y el resultado en la consola:

```Swift
// Crear una fecha
let fecha = Date()

// Declarar el formato de fecha
let formato = DateFormatter()
formato.dateFormat = "dd/MM/yyyy"

// Convertir la fecha en una cadena de texto
let fechaConvertida = formato.string(from: fecha)

// Imprimir la cadena resultante
print(fechaConvertida)

// Resultado: "26/05/2021"
```

¡Y eso es todo! Ahora ya puedes convertir cualquier fecha en una cadena de texto en Swift. ¡Pruébalo por ti mismo!

## Profundizando
Si quieres aprender más sobre cómo convertir fechas en cadenas de texto, aquí te dejamos algunos detalles adicionales a tener en cuenta:

- Puedes cambiar el idioma de la fecha mediante el uso del método `setLocalizedDateFormatFromTemplate()`.
- La clase `DateFormatter` también permite formatear fechas en diferentes zonas horarias usando `setTimeZone()`.
- Si deseas guardar la fecha en una base de datos, asegúrate de usar el mismo formato de fecha para evitar confusiones al convertirla nuevamente en una fecha en Swift.

## Ver también
- [Documentación de DateFormatter en la página de Apple](https://developer.apple.com/documentation/foundation/dateformatter)
- [Tutoriales de Swift en español](https://www.aprenderdev.com/tutoriales/swift/)