---
title:                "Interpolando una cadena"
html_title:           "Haskell: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Interpolación de cadena es una técnica en la programación para crear cadenas dinámicas combinando diferentes elementos. En Haskell, esto se logra utilizando la función `intercalate` del módulo `Data.List`. Los programadores utilizan esta técnica para generar mensajes personalizados o para construir cadenas complejas a partir de datos variables.

## Cómo hacerlo:
Para usar `intercalate`, primero debes importar el módulo `Data.List` en tu código. Luego, puedes usar la función en combinación con la función `show` para convertir valores en cadenas. Por ejemplo:

```Haskell
import Data.List (intercalate)

nombre = "Juan"
edad = 26

mensaje = intercalate " " ["Hola,", nombre ++ "! Tienes", show edad, "años."]
-- Salida: "Hola, Juan! Tienes 26 años."
```

## Profundizando:
La interpolación de cadenas se originó en el lenguaje de programación Perl y se ha vuelto popular en otros lenguajes como Ruby y Python. Alternativas en Haskell incluyen el módulo `Text.Printf` y la función `printf` de la biblioteca estándar. Estas soluciones también permiten la construcción de cadenas dinámicas, pero con diferentes sintaxis y características. En términos de implementación, la función `intercalate` en Haskell utiliza recursión para combinar los elementos de una lista en una sola cadena.

## Ver también:
- [Documentación del módulo `Data.List`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:intercalate)
- [Tutorial de Haskell en español](https://www.haskell-es.org/tutoriales/)