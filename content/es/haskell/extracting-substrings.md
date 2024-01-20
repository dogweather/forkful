---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracción de subcadenas en Haskell

## ¿Qué y por qué?

La extracción de subcadenas es el proceso de obtener segmentos específicos de una cadena más grande. Los programadores la utilizan para manipular y analizar datos en formato de texto con mayor eficacia.

## ¿Cómo se hace?

In Haskell, puedes utilizar las funciones de biblioteca estándar para extraer subcadenas de manera eficiente. Diferentes funciones te permiten extraer por posiciones, extraer prefijos o sufijos y más.

Aquí tienes algunas funciones de subcadena útiles:

Para extraer del principio de una cadena:

```Haskell
Prelude> take 5 "Hola mundo"
"Hola "
```

Para descartar del principio de una cadena:

```Haskell
Prelude> drop 5 "Hola mundo"
"mundo"
```

Para extraer del final de una cadena:

```Haskell
Prelude> reverse (take 5 (reverse "Hola mundo"))
"mundo"
```

## Deep Dive

La extracción de subcadenas ha sido una capacidad útil en la programación durante décadas y variable en todos los idiomas. Los primeros lenguajes de programación como COBOL y FORTRAN tenían capacidades integradas para extraer subcadenas.

Las alternativas a la extracción de subcadenas generalmente involucran el uso de expresiones regulares, que pueden ser más potentes pero también más complejas. Haskell soporta tanto la extracción de subcadenas como el uso de expresiones regulares.

Debajo del capó, la extracción de subcadenas en Haskell es extremadamente eficiente. Por supuesto, debes tener cuidado al hacerlo en cadenas muy grandes, ya que podría consumir demasiada memoria.

## Ver también

Para más detalles y tutoriales sobre la extracción de subcadenas en Haskell, puedes consultar estos recursos (en inglés):

- Real World Haskell, capítulo 8: Manipulación de texto (http://book.realworldhaskell.org/read/text.html)
- Learn You a Haskell for Great Good!: String (http://learnyouahaskell.com/starting-out#strings)
- Hoogle - Buscador de funcionalidad Haskell (https://hoogle.haskell.org/)