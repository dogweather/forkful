---
title:                "Haskell: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué

La extracción de subcadenas es una habilidad esencial en la programación Haskell. Permite a los programadores obtener partes específicas de una cadena de texto, lo que resulta útil en una variedad de escenarios, desde la manipulación de datos hasta la validación de entradas de usuario.

## Cómo hacerlo

En Haskell, podemos utilizar la función `take` para extraer una subcadena de una cadena dada. Por ejemplo:

```Haskell
take 5 "Hola Mundo"
```

Este código devuelve la subcadena "Hola ", ya que especificamos un número entero de 5 para indicar cuántos caracteres queremos extraer desde el principio de la cadena.

Otra función útil para extraer subcadenas es `drop`, que se utiliza para eliminar una cierta cantidad de caracteres desde el principio de una cadena. Por ejemplo:

```Haskell
drop 5 "Hola Mundo"
```

Este código devuelve la subcadena "Mundo", ya que hemos eliminado los primeros 5 caracteres de la cadena original.

También podemos usar `take` y `drop` juntas para obtener una subcadena específica dentro de una cadena. Por ejemplo:

```Haskell
take 3 (drop 5 "Hola Mundo")
```

Este código devuelve la subcadena "Mun", ya que primero eliminamos los primeros 5 caracteres y luego tomamos los siguientes 3.

## Profundizando

Además de las funciones `take` y `drop`, Haskell tiene otras funciones para extraer subcadenas, como `dropWhile` y `takeWhile`, que nos permiten extraer caracteres mientras se cumpla una condición dada. También podemos utilizar la función `splitAt` para dividir una cadena en dos partes en un índice dado.

Otra técnica interesante para extraer subcadenas es el uso de patrones y guardas en funciones recursivas. Esto nos permite extraer subcadenas de manera más dinámica y flexible.

## Ver también

- La documentación oficial de Haskell para `take`, `drop` y otras funciones de extracción de subcadenas: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#g:14
- Un tutorial en español sobre el uso de funciones de extracción de subcadenas en Haskell: https://josemanuelcr.com/blog/haskell/subcadenas-en-haskell/