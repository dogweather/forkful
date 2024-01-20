---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Convertir una cadena a minúsculas se refiere al proceso de cambiar todas las letras en mayúsculas de una cadena de texto a sus equivalentes en minúsculas. Los programadores suelen hacer esto para normalizar o estandarizar los datos durante el procesamiento del texto.

## Cómo:

Lua proporciona una función incorporada, `string.lower(str)` para convertir una cadena a minúsculas. Aquí te mostramos cómo se hace:

```Lua
-- Dada una cadena
local str = "¡Hola Mundo, Lua!"
-- Convertir a minúsculas
local lowerStr = string.lower(str)
-- Imprimir la cadena
print(lowerStr)
```

Salida:

```Lua
"¡hola mundo, lua!"
```

## Análisis en Profundidad:

### Contexto Histórico:

La conversión de texto a minúsculas es una práctica común en la programación de computadoras, que se remonta a los días de las primeras computadoras que sólo tenían letras en mayúsculas. Fue muy adoptada con la aparición de los sistemas basados en texto y los primeros motores de búsqueda.

### Alternativas:

Lua no proporciona una alternativa nativa para convertir una cadena a minúsculas. Sin embargo, puedes escribir una función para hacer esto manualmente si lo necesitas.

### Detalles de Implementación:

En Lua, `string.lower(str)` funciona recorriendo cada letra de la cadena `str`. Si la letra es una mayúscula, la convierte en minúscula. Los caracteres que no son letras se dejan sin cambios.

## Ver También:

1. [Guía de Programación de Lua](https://www.lua.org/pil/): Una guía completa del lenguaje de programación Lua.

2. [Documentación de Lua](https://www.lua.org/docs.html): Documentación oficial de Lua con una lista completa de funciones de cadenas y otros aspectos del lenguaje.

3. [String Manipulation (Manipulación de Cadenas)](https://www.lua.org/pil/20.html): Un análisis en profundidad de las diversas formas de manipular cadenas en Lua, incluyendo `string.lower(str)`.