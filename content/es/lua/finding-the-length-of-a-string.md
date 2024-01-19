---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Encontrar la longitud de una cadena es determinar el número de caracteres que contiene. Los programadores a menudo necesitan esta información para manipular cadenas de manera eficiente.

## Cómo hacerlo:
La longitud de una cadena en Lua se encuentra utilizando el operador de longitud `#`.

Aquí un simple ejemplo:
```Lua
cadena = "Hola Mundo"
print(#cadena)
```
Resultado:
```
10
```
En este ejemplo, `#cadena` devuelve 10 porque "Hola Mundo" contiene 10 caracteres.

## Inmersión Profunda

1. **Contexto histórico**: El lenguaje de programación Lua, originario de Brasil y lanzado en 1993, siempre ha tenido soporte para cadenas.

2. **Alternativas**: Aunque el operador de longitud `#` es el más común, también puedes usar la función `string.len()`.
```Lua
cadena = "Hola Mundo"
print(string.len(cadena))
```

Resultado:
```
10
```
3. **Detalles de la implementación**: En Lua, las cadenas son inmutables y cada cadena distinta se memoriza exactamente una vez. El operador `#` y `string.len()` devuelven rápidamente la longitud previamente calculada.

## Ver También
[Referencia Oficial de Lua](https://www.lua.org/manual/5.4/manual.html#3.4.7) | [Guía de Programación en Lua](https://www.lua.org/pil/11.1.html) | [Tutoriales de Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)