---
title:    "Elm: Buscando y reemplazando texto"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Una de las tareas comunes al programar es buscar y reemplazar texto en nuestro código. Esto puede ser útil para corregir errores, actualizar nombres de variables u otras acciones similares. Aprender cómo realizar esta tarea en Elm nos permitirá ser más eficientes y precisos en nuestro desarrollo.

## Cómo hacerlo

Para buscar y reemplazar texto en Elm, podemos utilizar la función `String.replace` que toma tres argumentos: la cadena original, el texto a buscar y el texto de reemplazo. Por ejemplo, si queremos reemplazar la palabra "perro" con "gato" en la cadena "Tengo un perro", podemos escribir el siguiente código:

```elm
String.replace "Tengo un perro" "perro" "gato"
```

Esto nos daría como resultado la cadena "Tengo un gato". También podemos almacenar el resultado en una variable para utilizarlo más adelante de la siguiente manera:

```elm
miCadena = String.replace "Tengo un perro" "perro" "gato"
```

Otra opción es utilizar la función `String.replaceRegex` que nos permite buscar y reemplazar texto utilizando expresiones regulares. Esta función toma cuatro argumentos: la cadena original, la expresión regular, el texto de reemplazo y un booleano que indica si queremos realizar el reemplazo en todas las coincidencias o solo en la primera. Por ejemplo, si queremos reemplazar todas las vocales en minúscula por la letra "a" en la cadena "Hola mundo", podemos escribir el siguiente código:

```elm
String.replaceRegex "Hola mundo" "[aeiou]" "a" True
```

Esto nos daría como resultado la cadena "Hala manga".

## Inmersión profunda

Además de buscar y reemplazar texto simple, también podemos utilizar esta función para realizar acciones más complejas en nuestras cadenas. Por ejemplo, si queremos reemplazar una palabra en un texto manteniendo su formato original, podemos utilizar `String.replace` junto con la función `String.toCase` que convierte una cadena en el mismo formato que otra. Por ejemplo, si queremos reemplazar la palabra "hola" por "adiós" en el texto "Hola, cómo estás?", podemos escribir el siguiente código:

```elm
String.replace "Hola, cómo estás?" "hola" (String.toCase "Hola" "adiós")
```

Esto nos daría como resultado la cadena "Adiós, cómo estás?". Además de esto, podemos utilizar funciones como `String.split` y `String.join` para reemplazar únicamente ciertas partes de una cadena.

## Ver también

- La documentación oficial de Elm sobre la función `String.replace`: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Ejemplos de uso de `String.replace` y `String.replaceRegex`: https://elmprogramming.com/replace-in-elm.html