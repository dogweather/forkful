---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Interpolar una cadena en programación significa insertar variables dentro de strings de texto en tiempo de ejecución. ¿Por qué? Para construir textos de forma dinámica y clara.

## Cómo hacerlo:

Haskell a priori no soporta interpolación de strings, pero módulos externos como `Text.Printf` o `Interpolate` sí lo hacen. Aquí un ejemplo simple:

```Haskell
import Text.Printf (printf)

main = do
  let nombre = "Ana"
  putStrLn $ printf "¡Hola, %s!" nombre
```

Correr este cógido produces la salida:

```bash
¡Hola, Ana!
```

## Viaje a lo más profundo

Se puede usar interpolación de strings en Haskell mediante módulos especiales - no se encuentra en el lenguaje por diseño pa' mantenerlo simple. Entre las alternativas están `Text.Printf` (desarrollado inicialmente en C) y `Interpolate`, que provee una sintaxis más moderna. No estrésese por la eficiencia: estos módulos son más que capaces para tareas generales de interpolación.

## Mira También

Visite estos enlaces para aprender más sobre interpolación en Haskell:

1. Text.Printf en Hackage: https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Printf.html
2. Módulo Interpolate: https://hackage.haskell.org/package/interpolate
3. Descripción de Interpolation en Haskell Wiki: https://wiki.haskell.org/Interpolation