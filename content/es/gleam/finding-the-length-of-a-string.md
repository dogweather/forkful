---
title:                "Gleam: Encontrando la longitud de una cadena"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?
La búsqueda de la longitud de una cadena es una tarea común y esencial en la programación. Saber la longitud de una cadena puede ayudarnos a manipularla de manera más eficiente y realizar operaciones específicas en ella.

## Cómo hacerlo

En Gleam, podemos encontrar fácilmente la longitud de una cadena utilizando la función `String.length`. Esta función toma una cadena como argumento y devuelve su longitud.

```Gleam
let cadena = "¡Hola mundo!"
let longitud = String.length(cadena)

$> longitud
13
```

También podemos utilizar un bucle `for` para recorrer cada carácter de la cadena y aumentar un contador cada vez que se encuentre uno.

```Gleam
let cadena = "¡Hola mundo!"
let contador = 0

for caracter in String.to_list(cadena) {
  contador = contador + 1
}

$> contador
13
```

Ambas opciones son sencillas y eficaces para encontrar la longitud de una cadena en Gleam.

## Profundizando

Cuando utilizamos la función `String.length`, Gleam en realidad cuenta la cantidad de caracteres en la cadena, incluyendo espacios en blanco, signos de puntuación y otros caracteres especiales.

Si queremos hacer un conteo más preciso y excluir ciertos caracteres, podemos utilizar la función `Regex.replace` para reemplazarlos por una cadena vacía y luego utilizar la función `String.length` en la cadena resultante.

```Gleam
use gleam/regex

let cadena = "¡Hola mundo!"
let cadena_sin_puntuacion = Regex.replace("[-¡!?,.;:¡¿?'\"¡]", cadena, "")

let longitud = String.length(cadena_sin_puntuacion)

$> longitud
9
```

## Ver también

- [Documentación de la función `String.length` en Gleam](https://gleam.run/documentation/std-lib-string.html#length)
- [Tutorial de Gleam](https://gleam.run/getting-started/introduction.html) para más información sobre la manipulación de cadenas.