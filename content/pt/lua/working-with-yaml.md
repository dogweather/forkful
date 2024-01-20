---
title:                "Trabalhando com YAML"
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que & Por quê?
YAML é uma linguagem de serialização de dados legível por humanos, frequentemente usada em configurações de projetos e transmissão de dados entre diferentes linguagens de programação. Programadores usam YAML por sua facilidade de leitura e compatibilidade com várias tecnologias.

## Como fazer:
Para manipular YAML em Lua, você vai precisar de uma biblioteca como o `lyaml`. Aqui está um exemplo de como carregar e imprimir dados de um arquivo YAML:

```Lua
local lyaml = require 'lyaml'
local file = io.open('config.yaml', 'r')
local data = lyaml.load(file:read('*a'))
file:close()

for key, value in pairs(data) do
  print(key .. ": " .. tostring(value))
end
```
Se `config.yaml` for:
```yaml
versao: 1.0
nome: MeuProjeto
```
A saída será:
```
versao: 1.0
nome: MeuProjeto
```

## Mergulho Profundo:
O YAML, criado em 2001, surgiu como uma alternativa ao XML para configurações e dados. Enquanto o JSON é uma alternativa popular devido à sua aderência ao JavaScript, o YAML é mais legível e pode representar mais complexidades, como comentários e referências. Em Lua, trabalhar com YAML geralmente envolve uma biblioteca de terceiros porque o núcleo da linguagem não tem suporte interno para YAML. 

## Veja Também:
- Documentação da biblioteca `lyaml`: [http://github.com/gvvaughan/lyaml](http://github.com/gvvaughan/lyaml)
- Especificação YAML: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)