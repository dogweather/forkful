---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:21.480911-07:00
description: "C\xF3mo hacerlo: En Lua, crear un arreglo asociativo (o una tabla, en\
  \ t\xE9rminos de Lua) es sencillo. Dejas de lado los \xEDndices num\xE9ricos habituales\
  \ por llaves\u2026"
lastmod: '2024-03-13T22:44:59.194894-06:00'
model: gpt-4-0125-preview
summary: "En Lua, crear un arreglo asociativo (o una tabla, en t\xE9rminos de Lua)\
  \ es sencillo."
title: Uso de matrices asociativas
weight: 15
---

## Cómo hacerlo:
En Lua, crear un arreglo asociativo (o una tabla, en términos de Lua) es sencillo. Dejas de lado los índices numéricos habituales por llaves de tu propia elección. Mira esto:

```Lua
-- Creando un arreglo asociativo
userInfo = {
  name = "Jamie",
  occupation = "Aventurero",
  level = 42
}

-- Accediendo a los elementos
print(userInfo["name"]) -- Imprime Jamie
print(userInfo.occupation) -- Imprime Aventurero

-- Añadiendo nuevos pares clave-valor
userInfo["hobby"] = "Programación"
userInfo.favLang = "Lua"

-- Iterando sobre el arreglo asociativo
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

Salida:
```
Jamie
Aventurero
name: Jamie
occupation: Aventurero
level: 42
hobby: Programación
favLang: Lua
```

¿La parte genial? Interactúas con los datos usando llaves significativas para ti, haciendo el código más legible y mantenible.

## Análisis Profundo
Cuando Lua apareció en escena, introdujo las tablas como una estructura de datos para todo uso, revolucionando cómo los desarrolladores manejan datos. A diferencia de en algunos lenguajes donde los arreglos asociativos y los arreglos son entidades distintas, las tablas de Lua sirven para ambos, simplificando el panorama de las estructuras de datos.

Lo que hace particularmente poderosas a las tablas de Lua es su flexibilidad. Sin embargo, esta flexibilidad viene con el costo potencial de implicaciones en el rendimiento, especialmente con grandes conjuntos de datos donde una estructura de datos más especializada podría ser preferible por eficiencia.

Aunque Lua no soporta nativamente estructuras de datos más convencionales de caja, como listas enlazadas o mapas hash, la adaptabilidad de la estructura de la tabla significa que puedes implementar estas usando tablas si necesitas hacerlo. Solo recuerda: con gran poder viene gran responsabilidad. Usa la flexibilidad sabiamente para mantener el rendimiento y la legibilidad de tu código.
