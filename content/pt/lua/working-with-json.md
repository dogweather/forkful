---
title:                "Trabalhando com json"
html_title:           "Lua: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-json.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Trabalhar com JSON (JavaScript Object Notation) é uma forma popular e eficiente de armazenar, transmitir e representar dados na linguagem de programação Lua. Programadores usam JSON porque ele é leve, fácil de entender e pode ser facilmente integrado com outras linguagens de programação e sistemas.

## Como fazer:

```Lua
-- Importando a biblioteca JSON
local json = require("json")

-- Criando uma tabela com dados
local pessoa = { nome = "João", idade = 30, cidade = "São Paulo" }

-- Convertendo a tabela para uma string JSON
local pessoaJSON = json.encode(pessoa)

-- Imprimindo a string JSON
print(pessoaJSON)

-- Saída: {"nome":"João","idade":30,"cidade":"São Paulo"}

-- Convertendo a string JSON de volta para uma tabela
local pessoa2 = json.decode(pessoaJSON)

-- Imprimindo os valores da tabela
print(pessoa2.nome)
print(pessoa2.idade)
print(pessoa2.cidade)

-- Saída:
-- João
-- 30
-- São Paulo
```

## Mergulho Profundo:

JSON foi criado no início dos anos 2000 como uma alternativa leve ao formato de dados XML. Ele é baseado na sintaxe de objetos e arrays do JavaScript, porém pode ser usado com várias linguagens de programação, incluindo Lua. Além de ser amplamente utilizado para transmitir e armazenar dados em sistemas web, o JSON também é comumente utilizado em APIs (Application Programming Interface) para receber e enviar dados entre diferentes sistemas.

Alguns dos principais formatos de dados alternativos ao JSON incluem XML, YAML, e CSV. Porém, o JSON é amplamente preferido devido à sua estrutura simples, legível e fácil de entender.

Para implementar o JSON em um projeto Lua, é necessário baixar e importar a biblioteca JSON disponível em repositórios online. Em seguida, é possível utilizar as funções `encode()` e `decode()` para converter entre tabelas e strings JSON. 

## Veja também:

- Documentação oficial do JSON para Lua: <http://luaforge.net/projects/json/>
- Tutorial de JSON em Lua: <https://www.tutorialspoint.com/lua/lua_json_encode.htm>
- Comparação entre diferentes formatos de dados: <https://www.quora.com/How-does-JSON-compare-to-XML-and-YAML>