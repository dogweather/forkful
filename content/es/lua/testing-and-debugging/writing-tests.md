---
title:                "Escribiendo pruebas"
aliases:
- /es/lua/writing-tests.md
date:                  2024-02-03T19:31:42.995104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Escribir pruebas en programación implica crear pequeñas piezas de código separadas para verificar automáticamente que diferentes partes de tu aplicación funcionen como se espera. Para los programadores de Lua, las pruebas aseguran la fiabilidad y ayudan a mantener la calidad del código, acelerando el proceso de depuración y haciendo que las modificaciones en la base de código sean más seguras.

## Cómo:

Lua, al ser un lenguaje de scripting ligero pero poderoso, no incluye un marco de pruebas incorporado. Sin embargo, bibliotecas de terceros como Busted y LuaUnit hacen que las pruebas sean relativamente sencillas. Aquí, veremos ejemplos usando ambos.

### Usando Busted

Busted es un marco de pruebas para Lua muy popular que ofrece una forma flexible de escribir pruebas. Primero, instala Busted a través de LuaRocks (el gestor de paquetes de Lua) con `luarocks install busted`. Una vez instalado, puedes escribir tus pruebas. Aquí hay una prueba simple para una función `add` que suma dos números:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Función de adición", function()
  it("debería sumar dos números correctamente", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Para ejecutar las pruebas, ejecuta `busted` en tu terminal. La salida de muestra para una prueba exitosa se vería así:

```
●
1 éxito / 0 fallos / 0 errores / 0 pendientes : 0.002 segundos
```

### Usando LuaUnit

LuaUnit es otro marco de pruebas que sigue las convenciones de xUnit y es fácil de configurar. Instala LuaUnit a través de LuaRocks usando `luarocks install luaunit`. Aquí te mostramos cómo podrías escribir una prueba similar a la anterior con LuaUnit:

```lua
-- add.lua permanece igual

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Ejecutar este script directamente a través de Lua (`lua test_add.lua`) producirá algo como:

```
.
Se ejecutaron 1 pruebas en 0.001 segundos, 1 éxito, 0 fallos
```

Tanto Busted como LuaUnit ofrecen amplias características para manejar varios escenarios de pruebas, incluyendo la simulación, espionaje y pruebas asíncronas. La elección entre ellos depende de las necesidades específicas de tu proyecto y tu preferencia personal en cuanto a sintaxis y funcionalidad.
