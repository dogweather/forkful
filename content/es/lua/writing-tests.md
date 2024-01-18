---
title:                "Escribiendo pruebas"
html_title:           "Lua: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas es una parte esencial del proceso de programación en Lua. Las pruebas son pequeños fragmentos de código que verifican si una determinada parte de nuestro programa funciona correctamente. Los programadores escriben pruebas para garantizar que su código es robusto y funciona como se espera.

## Cómo:

```Lua
-- Ejemplo 1: Prueba de una función de sumar
function sumar(a, b)
  return a + b
end

-- Prueba de la función sumar en caso de que la suma sea correcta
-- Se espera que el resultado sea 5
local resultado = sumar(2, 3)
assert(resultado == 5, "La suma está funcionando correctamente")

-- Ejemplo 2: Prueba de una función de división
function dividir(a, b)
  return a / b
end

-- Prueba de la función dividir en caso de que la división sea correcta
-- Se espera que el resultado sea 2
local resultado = dividir(10, 5)
assert(resultado == 2, "La división está funcionando correctamente")
```

## Profundizando:

Las pruebas no siempre han sido una práctica común en la programación, pero con el aumento de la complejidad de los programas y la necesidad de asegurar la calidad del código, se han convertido en una parte fundamental del proceso de desarrollo.

Existen diferentes tipos de pruebas que los programadores pueden realizar, como pruebas unitarias, de integración y de aceptación. También existen herramientas y frameworks específicos para escribir y ejecutar pruebas en Lua, como LuaUnit y busted.

Las pruebas también pueden ser una forma útil de documentación del código, ya que proporcionan ejemplos de cómo se espera que el código funcione. Esto puede ser especialmente útil para facilitar la colaboración entre diferentes programadores.

## Ver también:

- [LuaUnit](https://github.com/bluebird75/luaunit)
- [busted](https://github.com/Olivine-Labs/busted)