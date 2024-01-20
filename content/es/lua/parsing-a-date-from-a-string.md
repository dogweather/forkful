---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Interpretar una fecha desde una cadena es el proceso de traducir una fecha en forma de texto a un objeto Fecha en un lenguaje de programación. Los programadores lo hacen porque las fechas a menudo se reciben como cadenas, pero para manipularlas en el código, necesitamos convertirlas a objetos Fecha.

## Cómo:
Para interpretar una fecha en Lua, podemos usar la función `os.time`. Aquí hay un ejemplo:

```Lua
-- Cree un string que represente una fecha
local fecha_string = "07/31/2021 23:59:59"


-- Interpretar fecha
local pattern = "(%d+)%/(%d+)%/(%d+)%s*(%d+):(%d+):(%d+)"
local mes, dia, año, horas, minutos, segundos = fecha_string:match(pattern)
local fecha = os.time({year=año, month=mes, day=dia, hour=horas, min=minutos, sec=segundos })

-- Imprimir resultado
print(fecha)
```
La salida del código anterior será la representación de tiempo Unix de la Fecha.

## Profundizando:
Históricamente, diferentes lenguajes de programación han proporcionado diferentes métodos para interpretar fechas. En Lua, se utiliza principalmente la función os.time y las expresiones regulares, como en el ejemplo anterior.

Es importante saber que existen alternativas a este enfoque, dependiendo de tus necesidades. Por ejemplo, si te encuentras utilizando fechas a menudo en un proyecto más grande, puede que te convenga usar una biblioteca como `date.lua`, que proporciona una interfaz más rica para trabajar con fechas.

La implementación interna de las funciones de fecha en Lua es realmente una interfaz a las funciones de tiempo del sistema operativo subyacente. Por lo tanto, el comportamiento puede variar ligeramente entre diferentes sistemas operativos.

## Ver También:
1. Documentación del lenguaje Lua - [os.time](http://www.lua.org/manual/5.3/manual.html#6.9)
2. Lua: aprendiendo sobre fechas y tiempo - [Lua-date](https://github.com/Tieske/date)
3. Guía de Lua sobre expresiones regulares - [Cadenas](http://lua-users.org/wiki/PatternsTutorial)