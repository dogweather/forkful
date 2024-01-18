---
title:                "Trabajando con csv"
html_title:           "Lua: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con archivos CSV (Comma-Separated Values) en Lua se refiere a manipular datos tabulados en formato CSV y es una tarea común en la programación. Los programadores utilizan CSV para almacenar y exportar grandes cantidades de información estructurada, como resultados de bases de datos o registros de usuarios.

## Cómo hacerlo:
Aquí hay un ejemplo simple para leer un archivo CSV y mostrar su contenido:

```Lua
-- Primero, se carga la biblioteca CSV
local csv = require("csv")

-- Luego, se hace una llamada a la función read()
-- para leer el archivo especificado y almacenar su contenido en una variable
local data = csv.read("datos.csv")

-- Finalmente, se recorre y se imprime cada fila del archivo
for i, row in ipairs(data) do
  print("Fila " .. i .. ": " .. table.concat(row, ", "))
end
```

La salida debería ser una lista de filas del archivo, separadas por comas.

## Profundizando:
El formato CSV se remonta a los años 70 y fue inventado para facilitar el intercambio de datos entre hojas de cálculo. Aunque es ampliamente utilizado, tiene limitaciones como la falta de un estándar oficial y problemas con caracteres especiales.

Existen alternativas como JSON o XML para el almacenamiento de datos estructurados, pero CSV sigue siendo popular debido a su simplicidad y compatibilidad con muchos programas y lenguajes de programación.

En Lua, la biblioteca estándar incluye una función para leer archivos CSV, pero también hay bibliotecas externas más completas y personalizables disponibles.

## Ver también:
- [Documentación oficial de la biblioteca csv](https://github.com/FourierTransformer/lua-csv/blob/master/doc/csv.usage.md)
- [Biblioteca tercerizada con soporte para escribir archivos CSV](https://github.com/geoffleyland/lua-csv)