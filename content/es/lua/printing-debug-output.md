---
title:                "Imprimiendo salidas de depuración"
html_title:           "Lua: Imprimiendo salidas de depuración"
simple_title:         "Imprimiendo salidas de depuración"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Imprimir la salida de depuración es una herramienta útil para los programadores que les permite ver en tiempo real cómo se está ejecutando su código. Esto es especialmente útil cuando se está tratando con errores o intentando entender el flujo de un programa.

## Cómo hacerlo:

```Lua
-- La función "print" se utiliza para imprimir la salida en la consola
print("Hola Mundo!") -- Salida: Hola Mundo!

-- También se pueden imprimir variables y expresiones
local nombre = "María"
local edad = 28
print(nombre .. " tiene " .. edad .. " años.") -- Salida: María tiene 28 años.

-- La función "tostring" se puede utilizar para convertir variables en cadenas
print("El resultado de la suma es " .. tostring(5 + 3)) -- Salida: El resultado de la suma es 8

-- También se pueden imprimir tablas y sus valores
local paises = { "España", "Francia", "Italia", "Portugal" }
print(paises) -- Salida: table: 00426290
print(paises[2]) -- Salida: Francia
```

## Profundizando:

Imprimir la salida de depuración ha sido una práctica común entre los programadores desde los primeros días de la informática. Sin embargo, con el desarrollo de herramientas más avanzadas como depuradores y registradores, cada vez se utiliza menos. Sin embargo, sigue siendo una forma rápida y sencilla de entender cómo funciona un programa y solucionar posibles errores.

Otra alternativa para imprimir la salida de depuración es utilizar la función "error" que interrumpe la ejecución del programa y muestra un mensaje de error. Sin embargo, esto puede resultar molesto y poco práctico en programas más complejos.

En cuanto a la implementación, Lua proporciona la función "print" que se utiliza para imprimir la salida en la consola. Además, también se puede utilizar la función "io.write" para escribir directamente en el archivo de salida estándar sin agregar un salto de línea al final.

## Ver también:

- Función `print`: https://www.lua.org/manual/5.4/manual.html#pdf-print
- Función `error`: https://www.lua.org/manual/5.4/manual.html#pdf-error
- Función `io.write`: https://www.lua.org/manual/5.4/manual.html#pdf-io.write