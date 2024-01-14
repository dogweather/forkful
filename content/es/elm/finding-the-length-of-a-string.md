---
title:    "Elm: Encontrando la longitud de una cadena."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Cuando estamos programando, encontramos muchas situaciones en las que necesitamos conocer la longitud de una cadena de texto. Ya sea para validar una entrada de usuario, formatear datos o realizar cálculos, encontrar la longitud de una cadena es una tarea común y esencial en la programación. En este artículo, aprenderemos cómo encontrar la longitud de una cadena en Elm.

## Cómo hacerlo

En Elm, podemos encontrar la longitud de una cadena utilizando la función `String.length` que se encuentra en el módulo `String`. Para usarla, debemos declarar el módulo con la palabra clave `import` y luego llamar a la función con la cadena como argumento. Veamos un ejemplo en código:

```Elm
import String

miCadena = "Hola mundo"
longitud = String.length miCadena

-- La salida será: 11
```

En este ejemplo, primero importamos el módulo `String` y luego declaramos una cadena llamada `miCadena`. Por último, utilizamos la función `String.length` con la cadena como argumento y almacenamos su resultado en la variable `longitud`. Como resultado, obtendremos la longitud de la cadena, que en este caso será 11, ya que la cadena contiene 11 caracteres.

También podemos usar la función `length` directamente en una cadena sin necesidad de importar el módulo `String`:

```Elm
miOtraCadena = "¡Hola a todos!"
longitud = length miOtraCadena

-- La salida será: 14
```

En este ejemplo, también obtenemos la longitud de una cadena pero sin importar ningún módulo, ya que `length` es una función incluida por defecto en Elm.

## Profundizando

Cuando utilizamos la función `String.length` en una cadena que contiene caracteres especiales, puede haber algunas diferencias en la salida. Esto se debe a que en Elm, cada carácter se cuenta individualmente, incluyendo los acentos y caracteres especiales. Por ejemplo:

```Elm
cadena = "¡Hola ñoños!"
longitud = String.length cadena

-- La salida será: 13
```

En este caso, aunque la cadena parece tener 11 caracteres, la función `String.length` cuenta cada carácter individualmente, incluyendo la "ñ", por lo que el resultado será 13.

Otra cosa importante a tener en cuenta es que esta función solo funciona con cadenas de texto y no con otros tipos de datos como enteros o listas. En esos casos, se debe utilizar otra función específica para obtener la longitud.

## Ver también

- Documentación oficial de la función [String.length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- [Tutorial básico de Elm](https://elmprogramming.com/)
- [Guía de comandos en línea de Elm](https://guide.elm-lang.org/)