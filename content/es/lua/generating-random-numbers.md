---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:49:29.807089-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (¿Qué & Por Qué?)
Generar números aleatorios es crear valores que no pueden predecirse razonablemente. Los programadores los utilizan para juegos, simulaciones, pruebas, seguridad y cualquier lugar donde queramos imitar el azar.

## How to: (Cómo hacerlo:)
Para generar un número aleatorio en Lua, utilizarás la función `math.random()`. Puede producir un número entre 0 y 1, o dentro de un rango que definas.

```Lua
-- Inicializa el generador de números aleatorios
math.randomseed(os.time())

-- Genera un número aleatorio entre 0 y 1
local random_number = math.random()
print("Número aleatorio entre 0 y 1: " .. random_number)

-- Genera un número aleatorio entre 1 y 10
local random_range = math.random(1, 10)
print("Número aleatorio entre 1 y 10: " .. random_range)
```

Ejemplo de salida:
```
Número aleatorio entre 0 y 1: 0.0012512588885159
Número aleatorio entre 1 y 10: 7
```

## Deep Dive (Profundizando)
Históricamente, generar números aleatorios ha sido un desafío. En computación, usamos algoritmos para generar lo que llamamos pseudoaleatoriedad, ya que la aleatoriedad total es bastante difícil de alcanzar. En Lua, `math.random()` y `math.randomseed()` son las funciones que necesitas. Configura la semilla (`randomseed`) para variar la secuencia de números aleatorios cada vez que ejecutes tu programa.

Existen alternativas como números aleatorios provenientes de fuentes externas (verdaderamente aleatorias) o algoritmos más complejos disponibles en bibliotecas adicionales, por ejemplo, tú podrías usar la biblioteca `os`, para números aleatorios más impredecibles basados en el tiempo del sistema.

La implementación subyacente de la generación de números aleatorios en Lua depende del generador de números pseudoaleatorios de C (PRNG) que por defecto es bastante bueno para la mayoría de los casos, aunque no para criptografía.

## See Also (Mira También)
- La documentación de Lua sobre la biblioteca `math`: https://www.lua.org/manual/5.4/manual.html#6.7
- Un tutorial más detallado sobre números aleatorios en Lua: https://www.tutorialspoint.com/lua/lua_random_numbers.htm
- Información sobre generadores de números pseudoaleatorios (PRNG): https://en.wikipedia.org/wiki/Pseudorandom_number_generator