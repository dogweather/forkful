---
title:    "Kotlin: Convirtiendo una cadena a minúsculas"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas puede ser necesario en varios casos, como en la validación de entradas de usuario o en la comparación de strings sin importar mayúsculas y minúsculas. También puede ser una buena práctica de programación para mantener consistencia en tus datos.

## Cómo hacerlo

La forma más sencilla de convertir una cadena de texto a minúsculas en Kotlin es utilizando el método ```toLowerCase()```.
```
val nombre = "JUAN"
val nombreMin = nombre.toLowerCase()
// Output: juan
```
Ten en cuenta que este método devuelve una nueva cadena de texto, por lo que deberás asignarla a una nueva variable o reemplazar la cadena existente.

Otra opción es utilizar la función de extensión ```toLowerCase()``` en un objeto String:
```
val apellido = "PÉREZ"
val apellidoMin = apellido.toLowerCase()
// Output: pérez
```
También puedes utilizar el operador de asignación ```+=``` para concatenar el método ```toLowerCase()``` al final de la cadena:
```
var direccion = "AVENIDA LA PAZ"
direccion += direccion.toLowerCase()
// Output: AVENIDA LA PAZavenida la paz
```

## Profundizando

El método ```toLowerCase()``` utiliza la localización del sistema para realizar la conversión de mayúsculas a minúsculas. Esto significa que si estás utilizando el código en una región donde el idioma principal sea diferente del español, puede haber diferencias en cómo se realizan las conversiones. Puedes evitar esto utilizando la función de extensión ```toLowerCase(Locale)```, donde puedes especificar la localización que deseas utilizar.

Otra función útil es ```capitalize()```, que convierte la primera letra de la cadena a mayúscula:
```
val deporte = "fútbol"
val deporteCap = deporte.capitalize()
// Output: Fútbol
```

Si necesitas realizar operaciones con una cadena en minúsculas, se recomienda convertir la cadena original a minúsculas en lugar de utilizar el método ```toLowerCase()``` cada vez. Esto puede mejorar el rendimiento en casos de grandes cantidades de datos.

## Ver también

- [Documentación oficial de Kotlin sobre las funciones toLowerCase() y capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Más funciones de manejo de cadenas de texto en Kotlin](https://www.geeksforgeeks.org/kotlin-string-functions/)
- [Ejemplos de uso de cadenas de texto en Kotlin](https://beginnersbook.com/2018/03/kotlin-strings/)