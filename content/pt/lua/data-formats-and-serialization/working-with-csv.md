---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:29.529406-07:00
description: "Como fazer: Em Lua, trabalhar com arquivos CSV pode ser abordado usando\
  \ opera\xE7\xF5es b\xE1sicas de IO (Entrada/Sa\xEDda) de arquivos fornecidas pela\
  \ linguagem, sem\u2026"
lastmod: '2024-03-13T22:44:46.731395-06:00'
model: gpt-4-0125-preview
summary: "Em Lua, trabalhar com arquivos CSV pode ser abordado usando opera\xE7\xF5\
  es b\xE1sicas de IO (Entrada/Sa\xEDda) de arquivos fornecidas pela linguagem, sem\
  \ a necessidade de bibliotecas externas para tarefas simples."
title: Trabalhando com CSV
weight: 37
---

## Como fazer:
Em Lua, trabalhar com arquivos CSV pode ser abordado usando operações básicas de IO (Entrada/Saída) de arquivos fornecidas pela linguagem, sem a necessidade de bibliotecas externas para tarefas simples. Para operações mais complexas, como lidar com casos especiais (ex.: vírgulas dentro dos valores), pode ser benéfico usar bibliotecas de terceiros como `lua-csv`.

### Lendo um arquivo CSV
Aqui está um exemplo simples para ler um arquivo CSV linha por linha, dividindo cada linha em valores baseados no separador de vírgula.

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

**Saída de exemplo** (para um `example.csv` com conteúdo "name,age\newlineJohn Doe,30\newlineJane Doe,32"):
```
1	name
2	age
1	John Doe
2	30
1	Jane Doe
2	32
```

### Escrevendo um arquivo CSV
Para gerar um arquivo CSV, você simplesmente constrói strings com valores separados por vírgulas e os escreve em um arquivo linha por linha.

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

Isso criaria (ou sobrescreveria) um arquivo `output.csv` com os dados especificados.

### Usando lua-csv
Para um tratamento mais avançado de CSV, incluindo suporte para aspas e caracteres de escape, a biblioteca `lua-csv` é uma escolha robusta.

Primeiro, instale-a usando LuaRocks:
```shell
luarocks install lua-csv
```

Então, ler um arquivo CSV fica tão simples quanto:

```lua
local csv = require("csv")

-- Lendo de um arquivo
for fields in csv.open("example.csv") do
    for i, v in ipairs(fields) do
        print(i, v)
    end
end
```

E escrevendo em um CSV com aspas e escaping adequados:

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

Essa abordagem lida automaticamente com complexidades como vírgulas e aspas dentro dos valores.
