---
title:                "Trabalhando com JSON"
aliases: - /pt/lua/working-with-json.md
date:                  2024-02-03T19:23:20.099269-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Trabalhar com JSON em Lua envolve a análise de strings formatadas em JSON para tabelas Lua e vice-versa, possibilitando uma fácil troca de dados entre aplicações Lua e serviços web ou APIs externas. Programadores fazem isso para aproveitar o formato leve e fácil de analisar do JSON para armazenamento eficiente de dados, configuração ou comunicação com APIs.

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
