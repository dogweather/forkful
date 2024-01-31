---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Los archivos CSV contienen datos separados por comas, comunes en la importación y exportación de información, como contactos o estadísticas. Los programadores trabajan con CSV por ser un formato simple y ampliamente compatible para intercambiar datos entre diferentes sistemas y aplicaciones.

## Cómo hacerlo:
Para manejar un archivo CSV en Lua, primero lee el archivo y luego parsea los datos. Aquí un ejemplo rápido:

```Lua
local archivo = "datos.csv"
local datos_csv = {}

-- Lee el archivo CSV
local file = io.open(archivo, "r")
for linea in file:lines() do
    local valores = {}
    for valor in linea:gmatch("[^,]+") do
        table.insert(valores, valor)
    end
    table.insert(datos_csv, valores)
end
file:close()

-- Muestra los datos
for i, registro in ipairs(datos_csv) do
    for j, valor in ipairs(registro) do
        print("Registro "..i..", Valor "..j..": "..valor)
    end
end
```

Al correr este código con un archivo `datos.csv`, se mostraran los valores parseados en la consola.

## Inmersión Profunda:
Trabajar con CSV en Lua es directo, ya que no requiere bibliotecas externas. Originalmente diseñado en los años 70, el formato CSV es todavía relevante hoy. Hay alternativas, como JSON o XML, pero CSV sigue siendo el preferido para datos tabulares. Cuando se trata de archivos grandes o con datos complejos, se puede considerar usar el módulo `csvigo` o `LuaCSV` para funcionalidades adicionales.

## Ver También:
- Documentación de Lua sobre manejo de archivos: https://www.lua.org/pil/21.1.html
- LuaCSV GitHub: https://github.com/geoffleyland/lua-csv
- csvigo GitHub (desde Torch): https://github.com/clementfarabet/lua---csv
