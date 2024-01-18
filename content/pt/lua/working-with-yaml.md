---
title:                "Trabalhando com yaml"
html_title:           "Lua: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é e por que trabalhar com YAML?

YAML (Yet Another Markup Language) é uma linguagem de marcação utilizada por programadores para realizar a leitura e escrita de dados de forma estruturada e legível por humanos. É especialmente útil para configurações de sistemas e transferência de dados entre diferentes programas.

## Como fazer:

```Lua
-- Exemplo de como ler um arquivo YAML
local yaml = require("yaml")
local arquivo = io.open("dados.yaml", "r") -- abre o arquivo para leitura

-- lê o conteúdo do arquivo e converte para uma tabela em Lua
local dados = yaml.load(arquivo:read("*all"))

-- acessa os dados específicos da tabela
print(dados.nome) -- imprime "João"
print(dados.idade) -- imprime 25
print(dados.profissao) -- imprime "programador"

arquivo:close() -- fecha o arquivo

-- Exemplo de como escrever um arquivo YAML
local lista = { "banana", "maçã", "laranja" } -- cria uma tabela
local novo_arquivo = io.open("frutas.yaml", "w") -- abre o arquivo para escrita

-- converte a tabela para um formato YAML e escreve no arquivo
novo_arquivo:write(yaml.dump(lista))

novo_arquivo:close() -- fecha o arquivo
```

## Profundando:

YAML foi criado em 2001 por Clark Evans com o objetivo de ser uma alternativa mais simplificada e legível que o formato de dados JSON, e se tornou popular entre os programadores por sua facilidade de leitura e escrita. Além disso, existe uma grande variedade de bibliotecas em diversas linguagens de programação que oferecem suporte ao YAML, tornando sua utilização ainda mais versátil.

## Veja também:

- Documentação oficial do YAML: https://yaml.org/
- Biblioteca Lua-yaml: https://github.com/lubyk/lua-yaml