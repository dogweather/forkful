---
date: 2024-01-26 00:56:13.502573-07:00
description: "Manejar errores en la programaci\xF3n se trata de esperar lo inesperado.\
  \ Es el arte de planificar para cuando las cosas no salen como se espera y as\xED\
  \ poder\u2026"
lastmod: '2024-03-13T22:44:59.209332-06:00'
model: gpt-4-1106-preview
summary: "Manejar errores en la programaci\xF3n se trata de esperar lo inesperado."
title: Manejo de errores
weight: 16
---

## ¿Qué y Por Qué?
Manejar errores en la programación se trata de esperar lo inesperado. Es el arte de planificar para cuando las cosas no salen como se espera y así poder mantener tu programa funcionando sin problemas.

## Cómo hacerlo:
Lua usa dos funciones principales para el manejo de errores: `pcall` y `xpcall`. Así es como las utilizas:

```lua
function might_fail()
    if math.random() > 0.5 then
        error("¡Ups! Algo salió mal.")
    else
        print("¡Todo bien!")
    end
end

-- Usando pcall
local success, errorMessage = pcall(might_fail)

if success then
    print("¡Éxito!")
else
    print("Se atrapó un error:", errorMessage)
end

-- Usando xpcall con un manejador de errores
function myErrorHandler(err)
    print("El manejador de errores dice:", err)
end

local status = xpcall(might_fail, myErrorHandler)
print("¿Fue exitosa la llamada?", status)
```

La salida del ejemplo podría ser:

```
Se atrapó un error: ¡Ups! Algo salió mal.
El manejador de errores dice: ¡Ups! Algo salió mal.
¿Fue exitosa la llamada? false
```
O, si no ocurre ningún error:
```
¡Todo bien!
¡Éxito!
¡Todo bien!
¿Fue exitosa la llamada? true
```

## Análisis Profundo
Manejar errores, o "manejo de excepciones", no siempre fue algo común. Los programas antiguos se colgaban - mucho. A medida que la programación evolucionó, también lo hizo la necesidad de estabilidad. El enfoque de Lua es simple en comparación con algunos lenguajes. No hay bloques de `try/catch`, solo `pcall` y `xpcall`. El primero protege una llamada a función, devolviendo un estatus y cualquier error. El segundo añade una función de manejo de errores, útil para limpieza personalizada o registro.

Una alternativa en Lua es usar `assert`, que puede servir un propósito similar al lanzar un error si su condición es falsa. Pero no es tan flexible como `pcall` para escenarios complejos de manejo de errores.

Internamente, `pcall` y `xpcall` funcionan estableciendo un "entorno protegido" para que la función se ejecute. Si surge un error, el entorno lo capta y puede manejarlo de inmediato o pasarlo de vuelta para que el programa lo maneje.

## Vea También
- El libro Programming in Lua (tercera edición), disponible en https://www.lua.org/pil/ para una lectura exhaustiva sobre el manejo de errores (Sección 8.4).
- Manual de Referencia Oficial de Lua 5.4: https://www.lua.org/manual/5.4/ - para la información más actualizada sobre las funciones de manejo de errores de Lua.
- Wiki de usuarios de Lua sobre el manejo de errores: http://lua-users.org/wiki/ErrorHandling – para obtener perspectivas de la comunidad y patrones.
