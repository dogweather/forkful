---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Eliminar caracteres que coinciden con un patrón es el proceso de buscar y quitar caracteres específicos de una cadena. Los programadores realizan esta tarea para limpiar los datos, mejorar el formato o filtrar información innecesaria.

## Cómo:

En Haskell, utilizamos la función `filter` junto con las funciones `notElem` y `delete` del módulo `Data.List` para eliminar caracteres que coinciden con un patrón. Aquí tienes un ejemplo:

```Haskell
import Data.List

eliminarCaracteres :: String -> String -> String
eliminarCaracteres patrones texto = filter (`notElem` patrones) texto
```

Y así es como se ve en acción:

```Haskell
main = do
    let texto = "Desarrollo de Software en Haskell"
    let patrones = "aeiouAEIOU"
    putStrLn (eliminarCaracteres patrones texto)
```

La salida sería:

```Haskell
"Dsrll d Sftwr n Hskll"
```

## Inmersión profunda

Históricamente, el concepto de 'patrones de coincidencia' tiene sus raíces en el mundo de las expresiones regulares, con uso muy frecuente en lenguajes de programación como Perl y Ruby.

Alternativamente, puedes usar una comprensión de lista para conseguir el mismo resultado, aunque la legibilidad del código puede ser debatible:

```Haskell
eliminarCaracteres :: String -> String -> String
eliminarCaracteres patrones texto = [ c | c <- texto, c `notElem` patrones ]
```

La implementación de nuestra función es directa, usamos funciones de alto orden que aceptan funciones como argumentos. De esta manera, `filter` acepta la función `notElem`, la cual comprueba que un elemento no está contenido en la lista `patrones`.

## Ver también

Aquí tienes algunos recursos útiles para profundizar:

* [Introducción a Haskell](http://learnyouahaskell.com/chapters)
* [Documentación de la Biblioteca estándar de Haskell](https://www.haskell.org/onlinereport/standard-prelude.html)
* [Expresiones Regulares en Haskell](https://wiki.haskell.org/Regular_expressions)
* [El módulo Data.List de Haskell](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html)