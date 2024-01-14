---
title:                "Haskell: Encontrando la longitud de una cadena."
simple_title:         "Encontrando la longitud de una cadena."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo nos encontramos con la necesidad de encontrar la longitud de una cadena (string) en nuestro código. Ya sea para validar la entrada de usuario, formatear una salida o realizar operaciones complejas, conocer la longitud de una cadena es una habilidad básica que todo programador debería tener.

## Cómo hacerlo

En Haskell, hay varias formas de encontrar la longitud de una cadena. Una de ellas es utilizando la función `length` que viene incluida en el lenguaje. Veamos un ejemplo de cómo usarla:

```Haskell
cadena = "¡Hola Mundo!"
longitud = length cadena
```

En este código, primero definimos una variable `cadena` con el valor de la cadena que queremos medir. Luego, utilizamos la función `length` para encontrar su longitud y asignarla a una variable llamada `longitud`.

La función `length` toma como argumento una lista y devuelve un número entero que representa la cantidad de elementos en esa lista. En el caso de una cadena, cada caracter se cuenta como un elemento, por lo que la longitud resultante será igual al número de caracteres en la cadena.

Veamos ahora cómo sería la salida de este código ejecutado en la consola de Haskell:

```Haskell
Prelude> cadena = "¡Hola Mundo!"
Prelude> longitud = length cadena
Prelude> longitud
12
```

Como podemos ver, la longitud de la cadena "¡Hola Mundo!" es de 12 caracteres.

Otra forma de encontrar la longitud de una cadena es utilizando la función `genericLength` del módulo `Data.List`. Esta función es similar a `length`, pero puede trabajar con listas de cualquier tipo de dato numérico. Veamos un ejemplo:

```Haskell
import Data.List

cadena = "12345"
longitud = genericLength cadena
```

En este caso, la longitud resultante será de 5, ya que la lista contiene 5 elementos numéricos.

## Profundizando

Si queremos entender un poco más cómo funcionan estas funciones en Haskell, podemos analizar el código fuente de `length` y `genericLength`. Ambas son definidas en términos de la función `foldl`, que es una función de alto orden que toma una función, un valor inicial y una lista, y realiza una operación en esa lista.

Por ejemplo, el código de `length` se vería así:

```Haskell
length :: Foldable t => t a -> Int
length = foldl (\acc x -> acc + 1) 0
```

Este código indica que `length` toma una lista de tipo `t a` y devuelve un valor de tipo `Int`. Luego, utiliza `foldl` para aplicar una función que toma un acumulador (`acc`) y un elemento (`x`) y devuelve la suma del acumulador más 1. Finalmente, se le pasa como argumento 0 como el valor inicial del acumulador.

## Ver también

- [Documentación de la función length en Hoogle (en inglés)](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:length)
- [Tutorial sobre Haskell en español (en GitHub)](https://github.com/koalaman/sham/Haskell-Spanish-Translation)