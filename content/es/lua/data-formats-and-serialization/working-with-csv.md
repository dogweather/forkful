---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:27.783786-07:00
description: "C\xF3mo hacerlo: En Lua, trabajar con archivos CSV se puede abordar\
  \ utilizando operaciones b\xE1sicas de E/S de archivos proporcionadas por el lenguaje,\
  \ sin\u2026"
lastmod: '2024-03-13T22:44:59.225033-06:00'
model: gpt-4-0125-preview
summary: "En Lua, trabajar con archivos CSV se puede abordar utilizando operaciones\
  \ b\xE1sicas de E/S de archivos proporcionadas por el lenguaje, sin necesidad de\
  \ bibliotecas externas para tareas simples."
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:
En Lua, trabajar con archivos CSV se puede abordar utilizando operaciones básicas de E/S de archivos proporcionadas por el lenguaje, sin necesidad de bibliotecas externas para tareas simples. Para operaciones más complejas, como el manejo de casos especiales (por ejemplo, comas dentro de los valores), podría ser beneficioso usar bibliotecas de terceros como `lua-csv`.

### Leyendo un archivo CSV
Aquí hay un ejemplo simple para leer un archivo CSV línea por línea, dividiendo cada línea en valores basados en el separador de comas.

```lua
function parseCSVLine(line)
    local result = {}
    local from = 1
    local sep = ","
    local field
    while true do
        local start, finish = string.find(line, sep, from)
        if not start then
            table.insert(result, string.sub(line, from))
            break
        end
        field = string.sub(line, from, start - 1)
        table.insert(result, field)
        from = finish + 1
    end
    return result
end

local file = io.open("example.csv", "r")
for line in file:lines() do
    local values = parseCSVLine(line)
    for i, v in ipairs(values) do
        print(i, v)
    end
end
file:close()
```

**Salida de muestra** (para un `example.csv` con contenido "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Escribiendo un archivo CSV
Para generar un archivo CSV, simplemente construyes cadenas con valores separados por comas y los escribes en un archivo línea por línea.

```lua
local data = {
    {"name", "age"},
    {"John Doe", "30"},
    {"Jane Doe", "32"}
}

local file = io.open("output.csv", "w")
for _, v in ipairs(data) do
    file:write(table.concat(v, ","), "\n")
end
file:close()
```

Esto creará (o sobrescribirá) un archivo `output.csv` con los datos especificados.

### Usando lua-csv
Para un manejo más avanzado de CSV, incluyendo soporte para comillas y caracteres de escape, la biblioteca `lua-csv` es una elección robusta.

Primero, instálala usando LuaRocks:
```shell
luarocks install lua-csv
```

Luego, leer un archivo CSV se vuelve tan simple como:

```lua
local csv = require("csv")

-- Leyendo desde un archivo
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

Y escribir en un CSV con las comillas y escape adecuados:

```lua
local file = csv.open("output.csv", {write=true})

local data = {
    {"name", "profession", "location"},
    {"John Doe", "Software Engineer", "New York, NY"},
    {"Jane Doe", "Data Scientist", "\"San Francisco, CA\""}
}

for _, v in ipairs(data) do
    file:write(v)
end
```

Este enfoque maneja automáticamente complejidades como comas y comillas dentro de los valores.
