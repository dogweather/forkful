---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer los argumentos de la línea de comandos es simplemente tomar información ingresada por el usuario después de que se ejecutó el programa. Los programadores lo hacen para hacer que los programas sean más flexibles y efectivos.

## ¿Cómo hacerlo?

```Lua
-- Guarda los argumentos de la línea de comandos
arg_com = {...}

-- Recorre y muestra los argumentos
for i, v in ipairs(arg_com) do
   print("Arg " .. i .. " es " .. v)
end
```

Ejecute el script de arriba en su línea de comando de la siguiente manera:
```Lua
$ lua test.lua argumento1 argumento2
```

Y obtendrá esta salida:
```Lua
Arg 1 es argumento1
Arg 2 es argumento2
```

## Detalle

Los argumentos de la línea de comandos existen desde hace mucho tiempo. Usados en los sistemas operativos más antiguos como UNIX, han demostrado un método poderoso y flexible para interactuar con programas.

Alternativas para este método son los archivos de configuración o la interfaz gráfica de usuario (GUI). Sin embargo, los argumentos de la línea de comandos ofrecen ventajas en los casos que deseamos automatizar procesos o cuando desplegamos aplicaciones en ambientes sin GUI.

El uso de los argumentos de línea de comandos en Lua es bastante directo. La tabla "{...}" almacena los argumentos introducidos, que luego pueden ser iterados y utilizados como se necesite. Lua los indexa comenzando desde 1, a diferencia de otros lenguajes como C que empiezan con 0.

## Ver también

[Manual de Referencia de Lua](http://www.lua.org/manual/5.3/), [Tutorial Interactivo de Lua](https://www.learn-lua.org/), [Lectura de argumentos de línea de comando en Lua](https://www.tutorialspoint.com/lua/lua_command_line_arguments.htm)