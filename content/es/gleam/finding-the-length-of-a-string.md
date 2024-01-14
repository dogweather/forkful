---
title:    "Gleam: Encontrando la longitud de una cadena"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Por qué
Calcular la longitud de una cadena de texto es una tarea común en la programación y es esencial para muchos programas y aplicaciones. Al saber la cantidad de caracteres en una cadena, podemos validar entradas, formatear datos y realizar otras operaciones importantes.

# Cómo hacerlo
Para encontrar la longitud de una cadena en Gleam, podemos utilizar la función `length` seguida de la cadena a la que queremos medir su longitud. Por ejemplo:

```Gleam
let cadena = "¡Hola Mundo!"
let longitud = length(cadena)

// Salida: 12
```

En este ejemplo, asignamos la cadena "¡Hola Mundo!" a una variable llamada `cadena` y luego utilizamos la función `length` para obtener su longitud, que es 12. 

También podemos usar `length` directamente en una cadena sin asignarla primero a una variable:

```Gleam
let longitud = length("Gleam es un lenguaje de programación funcional")
// Salida: 45
```

# Profundizando
En Gleam, la función `length` utiliza la función de la biblioteca estándar `String.length`, que es una función de orden superior que toma una cadena y devuelve su longitud como un número entero. Esta función también se encarga de manejar correctamente los caracteres Unicode, por lo que puede manejar cadenas con caracteres especiales.

Una cosa importante a tener en cuenta es que `length` solo cuenta los *caracteres*, no los bytes o puntos de código. Esto significa que un solo carácter podría ocupar más de un byte, por lo que la longitud de una cadena puede ser mayor de lo esperado si se utilizan caracteres especiales o emojis.

# Ver también
- [Documentación oficial de la función `length`](https://gleam.run/documentation/standard-library/string.html#length)
- [Cálculo de longitud de cadena en otros lenguajes de programación](https://stackoverflow.com/questions/42962822/how-does-the-length-function-in-other-languages-count-characters-in-a-string)
- [Guía para entender y trabajar con caracteres Unicode](https://www.smashingmagazine.com/2021/07/guide-understanding-programming-unicode/)