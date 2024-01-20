---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer un archivo de texto en un acto de extraer información almacenada en dicho archivo. Los programadores lo hacen para manipular, analizar, o simplemente para usar los datos contenidos en los archivos de texto.

## Cómo Hacerlo

Aquí le muestro cómo leer un archivo de texto en Lua. Se trata simplemente de abrir el archivo en modo de lectura y luego leer la información contenida en él. Aquí está el código:

```Lua
file = io.open('archivo.txt', 'r') -- abre el archivo en modo de lectura
contenido = file:read('*all') -- lee todo el contenido del archivo
io.close(file) -- cierra el archivo
print(contenido) -- imprime el contenido
```

Si el contenido de su archivo.txt es ‘Hola Mundo’, la salida de este código será:

```
Hola Mundo
```

## Inmersión Profunda

El manejo de archivos en Lua ha sido una función fundamental desde sus primeras versiones. A pesar de que han surgido otros métodos alternativos con el tiempo, como el uso de bibliotecas para la manipulación de archivos XML o JSON, leer archivos de texto sigue siendo un método básico y confiable, especialmente para proyectos más pequeños que no necesitan capacidades avanzadas de manejo de datos.

En cuanto a los detalles de implementación, cuando abre un archivo en Lua, no está leyendo el contenido del archivo directamente en la memoria. Más bien, está creando un objeto de archivo que puede usar para leer los datos en partes, según sea necesario, lo cual es mucho más eficiente para archivos grandes.

## Ver También 

1. Manual de referencia de Lua para la entrada y salida de archivos: https://www.lua.org/manual/5.3/manual.html#6.8

2. Una introducción más detallada a la programación de archivos en Lua: http://lua-users.org/wiki/FileInputOutput