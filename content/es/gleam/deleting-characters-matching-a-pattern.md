---
title:    "Gleam: Borrando caracteres que coinciden con un patrón"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, cuando trabajamos con texto en programación, necesitamos eliminar ciertos caracteres que coinciden con ciertos patrones. Esto puede ser útil cuando se trabaja con inputs de usuarios, limpiando datos o formateando texto.

## Cómo hacerlo

En Gleam, podemos utilizar la función `String.replace` para eliminar un patrón específico de caracteres de una cadena de texto. Veamos un ejemplo:

```Gleam
let cadena = "Hola, ¡bienvenidos a mi blog!";
let nueva_cadena = String.replace(cadena, "!", "");
io.format("La nueva cadena es {}", [nueva_cadena]);
```

En este código, estamos usando la función `String.replace` para eliminar el símbolo de exclamación de la cadena original. La salida en la consola sería "La nueva cadena es Hola, ¡bienvenidos a mi blog".

Podemos ser más específicos y eliminar todos los caracteres que coinciden con un cierto patrón utilizando expresiones regulares. Por ejemplo, si queremos eliminar todos los caracteres numéricos de una cadena:

```Gleam
let cadena = "Hoy es 12 de octubre de 2021";
let nueva_cadena = String.replace(cadena, "[0-9]", "");
io.format("La nueva cadena es {}", [nueva_cadena]);
```

La salida sería "La nueva cadena es Hoy es de octubre de".

## Deep Dive

La función `String.replace` en Gleam también nos permite utilizar flags para realizar coincidencias globales o no sensibles a mayúsculas y minúsculas. Esto nos da un mayor control sobre cómo deseamos eliminar los caracteres que coinciden con el patrón.

Por ejemplo, si solo queremos eliminar el primer carácter que coincida con el patrón:

```Gleam
let cadena = "abcdefg";
let nueva_cadena = String.replace(cadena, "a", "", true);
```

Aquí, al utilizar el flag `true`, sólo se eliminará la primera letra "a" de la cadena original.

Para obtener una lista completa de los flags disponibles y su comportamiento, puedes consultar la documentación oficial de Gleam.

## Ver también

- [Documentación oficial de Gleam](https://gleam.run/documentation/)
- [Tutorial de expresiones regulares en Gleam](https://gleam.run/book/tutorials/regular-expressions.html)
- [Ejemplos de uso de `String.replace`](https://troutsblog.com/blog/deleting-patterns-string-replace-gleam)