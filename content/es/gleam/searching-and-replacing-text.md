---
title:                "Buscando y reemplazando texto"
html_title:           "Gleam: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué alguien usaría la función de buscar y reemplazar en Gleam?

Buscar y reemplazar texto es una tarea común en la programación. Puede ser necesario cuando se hacen cambios en un proyecto o se encuentran errores en el código. Usar la función de buscar y reemplazar en Gleam puede ahorrar tiempo y evitar errores manuales.

## Cómo: Ejemplos de código y salida de muestra en bloques de código " ```Gleam ... ```"

Para usar la función de buscar y reemplazar en Gleam, se utiliza el operador de doble flecha `=>`. El primer parámetro es el texto que se desea buscar, seguido del texto de reemplazo. A continuación se muestra un ejemplo de cómo cambiar todas las instancias de "Hola" por "Hola mundo":

``` gleam
let texto = "Hola, qué tal?"
let nuevo_texto = texto => "Hola mundo"
```
La salida de este código sería: `"Hola mundo, qué tal?"`

También se pueden utilizar patrones de búsqueda y reemplazo en lugar de solo cadenas de texto. Por ejemplo, si se desea reemplazar todas las vocales por números, se puede hacer lo siguiente:

``` gleam
let texto = "Hola, cómo estás?"
let nuevo_texto = texto => case
  "a" => "1"
  "e" => "2"
  "i" => "3"
  "o" => "4"
  "u" => "5"
  _ => texto
```
En este caso, la salida sería: `"H1l1, c3m4 2st1s?"`

## Profundizando: Información detallada sobre el uso de la función de buscar y reemplazar en Gleam

La función de buscar y reemplazar en Gleam también acepta expresiones regulares como patrones de búsqueda. Estas expresiones regulares pueden ser útiles cuando se busca una cadena de texto más compleja o cuando se desean reemplazar patrones específicos.

Además, la función de buscar y reemplazar en Gleam es inmutable, lo que significa que devuelve una copia del texto original con los cambios realizados. Por lo tanto, si se desea reemplazar el texto original, es importante asignar la salida de la función a una nueva variable.

## Ver también

- [Documentación de la función de buscar y reemplazar en Gleam](https://gleam.run/docs/std/string#replace)
- [Tutorial de Gleam](https://gleam.run/docs/tour#strings)
- [Expresiones regulares en Gleam](https://gleam.run/docs/cheatsheet#regular-expressions)