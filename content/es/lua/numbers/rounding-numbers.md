---
date: 2024-01-26 03:45:47.962019-07:00
description: "C\xF3mo hacerlo: Lua no incluye una funci\xF3n de redondeo de manera\
  \ predeterminada a diferencia de algunos otros lenguajes. Hist\xF3ricamente, necesitas\
  \ escribir\u2026"
lastmod: '2024-04-05T21:54:00.543313-06:00'
model: gpt-4-0125-preview
summary: "Lua no incluye una funci\xF3n de redondeo de manera predeterminada a diferencia\
  \ de algunos otros lenguajes."
title: "Redondeo de n\xFAmeros"
weight: 13
---

## Cómo hacerlo:
```lua
-- El redondeo básico en Lua no viene incorporado, pero puedes definir una función:

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- Para redondear a un lugar decimal específico:
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## Estudio Detallado
Lua no incluye una función de redondeo de manera predeterminada a diferencia de algunos otros lenguajes. Históricamente, necesitas escribir la tuya propia o usar una biblioteca de terceros. Las soluciones comunes dependen de `math.floor()` para el redondeo hacia abajo y de `math.ceil()` para el redondeo hacia arriba, acopladas con agregar o sustraer 0.5 antes de hacerlo, dependiendo del signo del número.

Las alternativas para crear tu propia función incluyen bibliotecas como "lua-users wiki" o "Penlight". Cada una tiene sus beneficios y compensaciones, como características adicionales o más sobrecarga.

Internamente, estas funciones normalmente funcionan aprovechando la forma en que las computadoras almacenan los números de punto flotante. Agregar 0.5 a un float positivo que deseas redondear lo empujará sobre el umbral del siguiente valor entero, así que cuando aplicas `math.floor()` se redondea hacia abajo al entero más cercano.

## Ver También
- [Manual de Referencia de Lua 5.4: Las Funciones Matemáticas](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Bibliotecas Lua de Penlight: Matemáticas](https://github.com/lunarmodules/Penlight)
