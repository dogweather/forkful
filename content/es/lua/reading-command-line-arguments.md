---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Lua: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Leer argumentos de línea de comando es una forma de obtener información del usuario a través de la línea de comandos o terminal mientras se ejecuta un programa. Los programadores suelen utilizar esta técnica para personalizar el comportamiento de sus programas, permitiendo al usuario ingresar opciones o datos específicos durante la ejecución.

## ¡Cómo hacerlo!
La lectura de argumentos de línea de comando en Lua se realiza a través de la función "arg", la cual almacena los argumentos ingresados por el usuario en un arreglo de tipo string. A continuación, se muestra un ejemplo de cómo imprimir los argumentos ingresados por el usuario:

```Lua
for i, v in ipairs(arg) do
  print(i, v)
end
```

Supongamos que ejecutamos nuestro programa con los siguientes argumentos: "hola", "mundo", "123". La salida sería:

```
1 hola
2 mundo
3 123
```

## Profundizando
La capacidad de leer argumentos de línea de comando es una característica útil que ha sido parte de Lua desde sus inicios. Sin embargo, también existen alternativas como el uso de archivos de configuración o la interacción con el usuario a través de una interfaz gráfica.

Internamente, la función "arg" utiliza la variable global "arg", la cual es creada al inicio de la ejecución del programa y contiene la lista de argumentos ingresados por el usuario. Es importante mencionar que el primer argumento siempre es el nombre del programa en sí.

## Vea también
- La documentación oficial de Lua sobre la función "arg": https://www.lua.org/manual/5.3/manual.html#pdf-arg
- Una guía detallada sobre cómo utilizar argumentos de línea de comando en Lua: https://www.tutorialspoint.com/lua/lua_command_line_arguments.htm