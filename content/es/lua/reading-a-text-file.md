---
title:                "Leyendo un archivo de texto"
html_title:           "Lua: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

¡Hola a todos los programadores de Lua! En este artículo, vamos a hablar sobre cómo leer un archivo de texto en Lua y por qué es útil para nosotros como programadores.

## ¿Qué y por qué?

Leer un archivo de texto simplemente significa acceder al contenido de un archivo de texto y utilizarlo en nuestro código. Los programadores lo hacemos principalmente para obtener información externa a nuestro programa, como datos de usuario, configuraciones o cualquier otro tipo de información que esté almacenada en un archivo de texto.

## Cómo hacerlo:

```Lua
-- Primero, abrimos el archivo utilizando la función io.open y guardamos el contenido en una variable
local archivo = io.open("ejemplo.txt", "r")

-- Ahora podemos usar un bucle para leer línea por línea del archivo e imprimir su contenido en la consola
for linea in archivo:lines() do
  print(linea)
end

-- Al terminar, debemos cerrar el archivo para liberar memoria
archivo:close()
```

El resultado en la consola sería algo así:

```
Estamos aprendiendo a leer un archivo de texto en Lua.
¿No es genial?
```

## Deep Dive:

En la historia de Lua, originalmente no había una forma integrada de leer archivos de texto. Sin embargo, con el tiempo se ha introducido la función io.open para facilitar esta tarea. Además, existen alternativas como la biblioteca LFS que ofrece una interfaz más completa para manipular archivos.

Si queremos ser más precisos en la lectura de un archivo de texto, podemos especificar el modo en el que abrimos el archivo (ej: solo lectura, lectura y escritura, etc.). También podemos utilizar la función io.read para leer una cantidad específica de bytes del archivo en lugar de línea por línea.

## Véase también:

Si quieres saber más sobre la lectura de archivos en Lua, puedes consultar estos recursos:

- [Documentación oficial de Lua sobre la función io.open](https://www.lua.org/manual/5.3/manual.html#pdf-io.open)
- [Biblioteca LFS](https://keplerproject.github.io/luafilesystem/manual.html)
- [Uso avanzado de io.open](https://riptutorial.com/es/lua/example/22360/uso-avanzado-de-io-open)

¡Eso es todo! Ahora ya sabes cómo leer un archivo de texto en Lua. ¡A seguir programando!