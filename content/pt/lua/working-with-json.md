---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?
JSON, ou JavaScript Object Notation, é um formato leve para troca de dados. Programadores o utilizam por sua facilidade de leitura e escrita, além de ser facilmente interpretável por máquinas.

## Como Fazer:
Para trabalhar com JSON em Lua, precisaremos da biblioteca `dkjson` ou outra similar.

```Lua
-- Primeiro, instale a dkjson: luarocks install dkjson
local json = require("dkjson")

-- Para codificar (Lua para JSON):
local tabela = { nome = "João", idade = 29, interesses = {"lua", "programação"} }
local str_json = json.encode(tabela)
print(str_json)  -- Saída: {"nome":"João","interesses":["lua","programação"],"idade":29}

-- Para decodificar (JSON para Lua):
local str_json = '{"nome":"João","interesses":["lua","programação"],"idade":29}'
local tabela, pos, err = json.decode(str_json)
if err then
  print("Erro:", err)
else
  print(tabela.nome)  -- Saída: João
end
```

## Mergulho Profundo
JSON surgiu a partir da linguagem JavaScript, mas hoje é independente e usado em várias linguagens de programação. Alternativas como XML também são usadas para o mesmo propósito, mas reparem que JSON é geralmente mais conciso. Em Lua, é necessário utilizar uma biblioteca externa para trabalhar com JSON, pois a linguagem não possui suporte nativo a essa formatação.

## Veja Também
- [Repositório da dkjson no GitHub](https://github.com/LuaDist/dkjson)
- [JSON](https://www.json.org/json-pt.html) - página oficial do JSON
- [LuaRocks](http://luarocks.org/) - gerenciador de pacotes para módulos Lua
