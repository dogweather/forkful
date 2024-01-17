---
title:                "Utilizando expresiones regulares"
html_title:           "Haskell: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?
Usar expresiones regulares es una forma de buscar y manipular patrones de texto en un programa Haskell. Los programadores a menudo lo utilizan para validar entradas de usuario, hacer búsquedas en bases de datos y realizar transformaciones en línea de texto.

# Cómo:
Los patrones de expresiones regulares se pueden especificar utilizando el módulo de texto "Text.Regex.Posix". Aquí hay un ejemplo de código que busca y reemplaza todas las vocales en una cadena con la letra 'x'.

```Haskell
import Text.Regex.Posix

-- Cadena de entrada
let input = "Hola mundo"

-- Patrón y reemplazo
let pattern = "[aeiou]"
let replacement = "x"

-- Función de búsqueda y reemplazo
let output = subRegex (mkRegex pattern) input replacement

-- Salida: Hxlx mxndx
```

# Profundizando:
Las expresiones regulares se basan en una teoría matemática desarrollada en la década de 1940 por el matemático Stephen Cole Kleene. Aunque Haskell tiene excelentes funciones integradas para manipular cadenas de texto, a veces es más eficiente y legible usar expresiones regulares. Alternativamente, también puede utilizar el paquete regex-pcre, que proporciona una mayor compatibilidad con otras implementaciones de expresiones regulares.

# Vea también:
- [Documentación oficial de expresiones regulares en Haskell](https://hackage.haskell.org/package/regex-posix)
- [Documentación oficial del paquete regex-pcre en Haskell](https://hackage.haskell.org/package/regex-pcre)