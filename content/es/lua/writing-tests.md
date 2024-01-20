---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas es crear escenarios para verificar que tu código hace lo que debe. Los programadores prueban para encontrar errores antes de que el código llegue a producción, asegurando calidad y evitando dolores de cabeza.

## Cómo hacerlo:

Imagina que tienes una función para sumar números. Así es como podrías escribir una prueba simple:

```Lua
function sum(a, b)
    return a + b
end

-- Prueba
local result = sum(2, 3)
assert(result == 5, "La suma de 2 y 3 debería ser 5")
```

Si la prueba falla, verás un mensaje de error. Si pasa, no tendrás ningún output, lo cual indica éxito.

## Profundizando

Históricamente, las pruebas se hacían manualmente, pero hoy en día existen herramientas como Busted o LuaUnit para automatizarlas. Estas herramientas permiten organizarte mejor y ofrecen opciones más avanzadas para casos de prueba. Con Lua, puedes integrar pruebas en tu flujo de trabajo para asegurar que los cambios no rompan funcionalidades existentes.

## Ver También:

- Documentación de LuaUnit: [https://luaunit.readthedocs.io/en/latest/](https://luaunit.readthedocs.io/en/latest/)
- Guía para pruebas unitarias en Lua: [https://www.lua.org/pil/contents.html#part4](https://www.lua.org/pil/contents.html#part4)