---
title:                "Usando un depurador"
aliases: - /es/lua/using-a-debugger.md
date:                  2024-01-26T03:50:37.554018-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Un depurador es una herramienta que te permite inspeccionar y controlar la ejecución de un programa, facilitando la identificación de donde las cosas van mal. Los programadores utilizan depuradores para eliminar errores, entender el flujo del código y asegurar que su código esté limpio como un silbato.

## Cómo hacerlo:
Lua no viene con un depurador integrado, pero puedes usar otros externos, como ZeroBrane Studio. Aquí tienes una muestra de cómo trabajarías con él:

```Lua
-- Este es un script Lua simple con un error intencional
local function add(a, b)
    local result = a + b -- Vaya, fingamos que olvidamos definir 'b'
    return result
end

print(add(10))
```

Cuando ejecutas esto en un depurador, detendrá la ejecución donde las cosas se complican. Verás algo como esto:

```
lua: example.lua:3: intento de realizar aritmética con un valor nulo (local 'b')
rastreo de pila:
	example.lua:3: en la función 'add'
	example.lua:7: en el bloque principal
	[C]: en ?
```

Puedes establecer puntos de interrupción, avanzar paso a paso por tu código y echar un vistazo a los valores de las variables para rastrear el error sin perder la cordura.

## Inmersión Profunda
Lamentablemente, la simplicidad de Lua no se extiende a la depuración. Sin embargo, no hay de qué preocuparse, la comunidad de Lua te respalda. Herramientas como ZeroBrane Studio, LuaDec y otras ofrecen capacidades de depuración. Históricamente, los depuradores existieron poco después de que los primeros programas fallaran, dando a los desarrolladores los medios para arreglar su código sin manipularlo a ciegas.

Con Lua, a menudo dependes de depuradores externos o los integras en tu entorno de desarrollo. ZeroBrane Studio, por ejemplo, es un IDE que integra completamente un depurador de Lua. Te permite avanzar paso a paso por el código, establecer puntos de interrupción y observar variables. En el lado de la implementación, los depuradores suelen usar ganchos para insertar puntos de interrupción y otras facilidades de depuración.

¿Alternativas? Por supuesto. Los buenos y viejos comandos de impresión, conocidos cariñosamente como "depuración printf", a veces pueden hacer el truco sin herramientas sofisticadas.

## Ver También
Para continuar tu viaje de depuración, consulta:

- ZeroBrane Studio: https://studio.zerobrane.com/
- Wiki de usuarios de Lua sobre depuración de código Lua: http://lua-users.org/wiki/DebuggingLuaCode
- La referencia de la biblioteca `debug` en el manual de Lua: https://www.lua.org/manual/5.4/manual.html#6.10
