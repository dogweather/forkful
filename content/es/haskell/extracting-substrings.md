---
title:    "Haskell: Extrayendo subcadenas"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## ¿Por qué extraer subcadenas?

Extraer subcadenas es una tarea común en la programación, especialmente en el procesamiento de texto y cadenas de caracteres. Puede ser útil cuando se necesita manipular una parte específica de una cadena o cuando se desea obtener una subcadena de una cadena más grande.

## Cómo hacerlo

Para extraer subcadenas en Haskell, utilizamos la función `take` y `drop` que están disponibles en el módulo `Prelude`. Estas funciones toman una subcadena de una cadena más grande, especificando la posición inicial y la cantidad de caracteres a tomar o dejar.

```Haskell
-- Definimos una cadena
let cadena = "¡Hola Haskell!"

-- Extraemos los primeros 4 caracteres de la cadena
take 4 cadena  -- Salida: "¡Hola"

-- Extraemos los últimos 7 caracteres de la cadena
drop 5 cadena -- Salida: "Haskell!"
```

También se puede utilizar la función `splitAt`, que toma una cadena y un número como parámetros y divide la cadena en dos subcadenas en la posición especificada.

```Haskell
splitAt 5 cadena -- Salida: ("¡Hola", " Haskell!")
```

## Inmersión profunda

Además de las funciones mencionadas anteriormente, también existen otras formas de extraer subcadenas en Haskell utilizando patrones y expresiones regulares. Estos métodos pueden ser más avanzados, pero pueden ser útiles en situaciones específicas donde la manipulación de cadenas es más compleja.

## Ver también

- [Documentación oficial de Haskell sobre la función `take` y `drop`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#g:18)
- [Tutorial de Haskell sobre patrones y expresiones regulares](https://www.tutorialspoint.com/haskell/haskell_pattern_matching.htm)
- [Librería de Haskell para trabajar con expresiones regulares](https://hackage.haskell.org/package/regex-compat-tdfa)