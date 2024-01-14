---
title:                "Elm: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por qué

A veces necesitamos saber cuántos caracteres tiene una palabra o una oración. Ya sea para validar una entrada de usuario o para mostrar un resumen de un texto largo, encontrar la longitud de una cadena es una tarea común en la programación. En esta publicación de blog, aprenderás cómo hacerlo en Elm de manera sencilla y eficiente.

## Cómo hacerlo

En Elm, podemos encontrar la longitud de una cadena utilizando la función `String.length`. Esta función toma una cadena como argumento y devuelve un número entero que representa la cantidad de caracteres en esa cadena.

```Elm
-- Definir una cadena
miCadena = "¡Hola Mundo!"

-- Encontrar la longitud de la cadena
resultado = String.length miCadena

-- Salida: 12
```

Es importante tener en cuenta que los espacios en blanco y los caracteres especiales también cuentan como caracteres en una cadena. Por ejemplo, la cadena `"¡Hola Mundo!"` tiene 12 caracteres, incluyendo el espacio y el signo de exclamación.

También podemos utilizar la función `length` directamente en una cadena sin necesidad de definirla previamente.

```Elm
-- Encontrar la longitud de una cadena directamente
resultado = String.length "¡Hola Mundo!"

-- Salida: 12
```

Otra forma útil de encontrar la longitud de una cadena es utilizando la función `toString`. Esta función convierte cualquier valor en una cadena y luego podemos encontrar su longitud utilizando `String.length`.

```Elm
-- Definir un número
miNumero = 123456

-- Convertir el número a una cadena y encontrar su longitud
resultado = String.length (toString miNumero)

-- Salida: 6
```

## Profundizando

En realidad, detrás de la función `String.length` hay una operación más compleja que involucra gráficos de código. Esto se debe a que en Elm, las cadenas se almacenan internamente como listas de caracteres, y la función de longitud simplemente cuenta la cantidad de elementos en esa lista.

Además, es importante tener en cuenta que la función `String.length` es sensible a la codificación de caracteres. Esto significa que en algunas situaciones puede devolver valores diferentes para la misma cadena, dependiendo de la codificación utilizada.

## Ver también

- [Documentación de Elm: String](https://elm-lang.org/docs/strings)
- [Blog de Elm: Manipulando Cadenas de Texto](https://elmprogramming.com/strings.html)
- [Tutorial de Elm: Gestión de Entradas de Usuario](https://guide.elm-lang.org/effects/text_fields.html)