---
title:    "Gleam: Extrayendo subcadenas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por qué
 Extraer subcadenas o subconjuntos de una cadena de texto es una tarea común en la programación. Puede ser útil para separar información específica de una cadena o para manipular datos de manera más eficiente. En este artículo, exploraremos cómo podemos hacer esto utilizando la programación en Gleam. 

## Cómo hacerlo
En Gleam, podemos usar la función `String.substr` para extraer subcadenas de una cadena de texto. Esta función toma dos parámetros: el índice de inicio y el índice de fin de la subcadena que queremos extraer. Veamos un ejemplo de cómo extraer la palabra "Gleam" de una cadena de texto:

``` Gleam
let texto = "¡Hola desde Gleam!"
let subcadena = String.substr(texto, 11, 16)
```

En este ejemplo, el parámetro `11` es el índice de inicio y el `16` es el índice de fin, que corresponden a la posición de los caracteres "G" y "m" en la cadena original. La variable `subcadena` ahora contendrá la subcadena "Gleam". 

También podemos usar la función `String.slice` para extraer subcadenas de forma similar, pasando el índice de inicio y el número de caracteres que queremos extraer. Veamos otro ejemplo:

``` Gleam
let texto = "Este artículo es de Gleam"
let subcadena = String.slice(texto, 19, 5)
```

En este caso, la subcadena resultante sería "Gleam" nuevamente. 

## Profundizando
Si queremos ser más específicos al extraer subcadenas, podemos usar expresiones regulares en lugar de índices de inicio y fin. Por ejemplo:

``` Gleam
let texto = "El número de teléfono es +123-456-789"
let regex = regex.new("\\+[0-9]-[0-9]{3}-[0-9]{3}-[0-9]{3}")
let subcadena = regex.match(texto) |> regex.captures |> List.get(0)
```

En este ejemplo, hemos creado una expresión regular para encontrar el número de teléfono en el formato especificado. Luego, usamos el método `match` para encontrar la primera coincidencia en la cadena `texto`. Finalmente, usamos el método `captures` para obtener la lista de subcadenas coincidentes y la función `List.get` para obtener la primera subcadena (que en este caso sería el número de teléfono). 

## Ver también
- [Documentación de la función `String.substr` en la web de Gleam](https://gleam.run/std/string.html#substr)
- [Documentación de la función `String.slice` en la web de Gleam](https://gleam.run/std/string.html#slice)
- [Documentación de expresiones regulares en Gleam](https://gleam.run/book/standard-library.html#regular-expressions)