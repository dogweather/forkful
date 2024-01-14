---
title:                "Haskell: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
En la programación, a menudo nos encontramos con la necesidad de buscar y reemplazar texto en nuestro código. Esto puede ser debido a errores ortográficos, cambios en los requerimientos del proyecto, o simplemente para hacer nuestro código más legible. Afortunadamente, Haskell tiene una función incorporada que nos permite realizar esta tarea de una manera eficiente y confiable.

## Cómo hacerlo
La función que utilizaremos para buscar y reemplazar texto en Haskell se llama `replace`. Esta función acepta tres parámetros: el texto a buscar, el texto de reemplazo y el texto original. Veamos un ejemplo de cómo usarlo:

```Haskell
replace "perro" "gato" "Me gustan los perros"
```

Este código nos devolverá la cadena "Me gustan los gatos". Como puedes ver, la función `replace` es muy fácil de usar y nos ahorra mucho tiempo en la edición de texto en nuestro código.

Otra forma de utilizar esta función es con el uso de variables. Por ejemplo:

```Haskell
let palabraABuscar = "adiós"
let palabraDeReemplazo = "hola"
let frase = "¡adiós amigos!"
replace palabraABuscar palabraDeReemplazo frase
```

En este caso, obtendremos como resultado la cadena "¡hola amigos!". Como ves, podemos utilizar variables para hacer la función más reusable y dinámica.

## Profundizando
La función `replace` en realidad es una función de orden superior, lo que significa que también puede tomar una función como parámetro. Esto nos permite hacer reemplazos más complicados y personalizados. Por ejemplo, podríamos utilizar la función `map` para reemplazar cada carácter en una cadena por otro de manera específica.

Otra característica interesante de `replace` es que también funciona con tipos de datos más complejos, como listas o tuplas. Esto nos permite buscar y reemplazar no solo texto, sino también listas de números o tipos de datos personalizados.

## Ver también
- Documentación oficial de la función `replace` en Haskell: https://www.haskell.org/hoogle/?hoogle=replace
- Ejemplos de uso avanzado de la función `replace`: https://wiki.haskell.org/List_function_proposals_by_example%2B
- Tutoriales de Haskell para principiantes: https://www.haskell.org/tutorial/