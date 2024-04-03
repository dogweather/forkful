---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:20.099269-07:00
description: "Como fazer: Lua n\xE3o inclui uma biblioteca interna para processamento\
  \ de JSON. Portanto, uma das bibliotecas de terceiros populares \xE9 a `dkjson`,\
  \ que voc\xEA\u2026"
lastmod: '2024-03-13T22:44:46.730357-06:00'
model: gpt-4-0125-preview
summary: "Lua n\xE3o inclui uma biblioteca interna para processamento de JSON."
title: Trabalhando com JSON
weight: 38
---

## Como fazer:
Lua não inclui uma biblioteca interna para processamento de JSON. Portanto, uma das bibliotecas de terceiros populares é a `dkjson`, que você pode usar facilmente para codificação e decodificação de JSON. Primeiro, certifique-se de instalar o `dkjson`, por exemplo, através do LuaRocks (`luarocks install dkjson`), e então siga os exemplos abaixo.

### Decodificando JSON para Tabela Lua
```lua
local dkjson = require "dkjson"

local jsonString = '{"name": "Programador Lua", "age": 30, "languages": ["Lua", "JavaScript"]}'
local luaTable, pos, err = dkjson.decode(jsonString, 1, nil)
if err then
  print ("Erro:", err)
else
  print("Nome:", luaTable.name) -- Saída: Nome: Programador Lua
  print("Idade:", luaTable.age) -- Saída: Idade: 30
  print("Linguagens:", table.concat(luaTable.languages, ", ")) -- Saída: Linguagens: Lua, JavaScript
end
```

### Codificando Tabela Lua para JSON
```lua
local dkjson = require "dkjson"

local luaTable = {
  name = "Programador Lua",
  age = 30,
  languages = { "Lua", "JavaScript" }
}

local jsonString = dkjson.encode(luaTable, { indent = true })
print(jsonString)
```

Saída de exemplo para codificação:
```json
{
  "age": 30,
  "languages": [
    "Lua",
    "JavaScript"
  ],
  "name": "Programador Lua"
}
```

Estes exemplos simples demonstram como trabalhar com JSON em Lua, facilitando a integração de aplicações Lua com várias tecnologias web e APIs externas. Lembre-se, enquanto `dkjson` é usado nesses exemplos, outras bibliotecas como `cjson` e `RapidJSON` também podem ser alternativas adequadas dependendo das necessidades do seu projeto.
