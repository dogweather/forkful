---
title:    "Haskell: Encontrando la longitud de una cadena"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por qué

En la programación, a menudo necesitamos trabajar con cadenas de texto. Y una de las operaciones más comunes que realizamos con estas cadenas es encontrar su longitud. En este artículo, hablaremos sobre cómo podemos hacer esto en Haskell.

# Cómo hacerlo

Hay varias formas de encontrar la longitud de una cadena en Haskell. Una manera simple es usando la función `length`, que toma una cadena como argumento y devuelve su longitud como un número entero.

```Haskell
length "Hola Mundo"
-- Salida: 10
```

También podemos usar la función `length` en una lista de caracteres para obtener la misma resultado.

```Haskell
let lista = ['H', 'o', 'l', 'a', ' ', 'M', 'u', 'n', 'd', 'o']
length lista
-- Salida: 10
```

Otra manera de encontrar la longitud de una cadena es usando `Data.Text`, una librería estándar de Haskell que nos permite trabajar con texto de manera eficiente. Para usarla, primero necesitamos importarla en nuestro código.

```Haskell
import Data.Text as T
```
Y luego podemos utilizar la función `T.length`, que funciona de la misma manera que `length`.

```Haskell
T.length "Hola Mundo"
-- Salida: 10
```

# Inmersión profunda

Si nos adentramos en el código de la función `length`, encontraremos que está implementada de la siguiente manera:

```Haskell
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs
```

Pero, ¿qué significan esas líneas? La primera línea indica que la función toma una lista de algún tipo `a` y devuelve un número entero (`Int`). La segunda línea es el caso base, que nos dice que si la lista está vacía, entonces su longitud es 0. Y la tercera línea es la recursión, lo que significa que podemos obtener la longitud de una lista no vacía sumando 1 y llamando a la función de nuevo con el resto de la lista.

Esta implementación es una forma eficiente de encontrar la longitud de una lista, ya que no requiere recorrer la lista por completo. En cambio, va eliminando elementos uno por uno hasta llegar al caso base.

# Ver también

- [Documentación oficial de `length`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:length)
- [Documentación de `Data.Text`](https://hackage.haskell.org/package/text-1.2.3.2/docs/Data-Text.html)
- [Tutorial de Haskell](https://www.haskell.org/tutorial/index.html)