---
date: 2024-01-20 17:48:02.953625-07:00
description: "C\xF3mo hacerlo: Salida de muestra."
lastmod: '2024-04-05T21:54:00.539279-06:00'
model: gpt-4-1106-preview
summary: ''
title: Calculando la longitud de una cadena
weight: 7
---

## Cómo hacerlo:
```Lua
local texto = "Hola Mundo"
local longitud = #texto
print("La longitud de la cadena es: " .. longitud)
```

Salida de muestra:

```
La longitud de la cadena es: 10
```

Puedes usar también la función `string.len()`:

```Lua
local texto = "Adiós"
local longitud = string.len(texto)
print("La longitud de la cadena es: " .. longitud)
```

Salida de muestra:

```
La longitud de la cadena es: 5
```

## Inmersión Profunda
Históricamente, el manejo de cadenas ha sido una parte crucial en la programación. Lua proporciona una manera fácil y directa de encontrar la longitud de una cadena: el operador de longitud `#`. Lua guarda las cadenas en forma interna con una longitud conocida, lo que permite que esta operación sea muy rápida. 

Una alternativa en lenguajes más antiguos sería iterar a través de la cadena hasta encontrar un caracter de terminación, lo cual se compara con el operador `#` de Lua, que es mucho más eficiente.

Además, Lua soporta también cadenas que contienen ceros que no actúan como terminadores como en C. Esto significa que Lua puede manejar cualquier dato binario en una cadena.

Si tienes una tabla que representa una cadena (por ejemplo, cada carácter es un elemento en la tabla), usarás `table.getn()` o `#` para obtener la "longitud" de esa tabla.

## Ver También
- Documentación oficial de Lua - Strings: https://www.lua.org/manual/5.4/manual.html#6.4
- Tutorial de Lua - Strings: https://www.tutorialspoint.com/lua/lua_strings.htm
- Foro de preguntas y respuestas de Lua, para aclarar dudas sobre cadenas: https://stackoverflow.com/questions/tagged/lua+string
