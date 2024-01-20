---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Expressões regulares são padrões usados para encontrar correspondências específicas em textos. Programadores as utilizam para validar, buscar, substituir e analisar dados de forma eficiente e rápida.

## Como fazer:
```Lua
local texto = "Hoje é dia 04/03/2023."
-- Encontrando uma data no formato dd/mm/aaaa
local padrao = "(%d%d)/(%d%d)/(%d%d%d%d)"
local dia, mes, ano = string.match(texto, padrao)
print("Dia encontrado:", dia)
print("Mês encontrado:", mes)
print("Ano encontrado:", ano)
```
Saída:
```
Dia encontrado: 04
Mês encontrado: 03
Ano encontrado: 2023
```

Para substituir texto:
```Lua
local frase = "Lua é divertido!"
local nova_frase = string.gsub(frase, "divertido", "incrível")
print(nova_frase)
```
Saída:
```
Lua é incrível!
```

## Aprofundamento
Expressões regulares surgiram na década de 1950, com base nos trabalhos de teoria de linguagem formal e automata. Em Lua, as expressões regulares são implementadas através de padrões, que são simplificações das regex encontradas em outras linguagens. Alternativas incluem o uso de bibliotecas externas como Lrexlib ou LPeg, que permitem regex mais complexas e com recursos avançados.

## Ver Também
- [Lua 5.4 Reference Manual (Patterns)](https://www.lua.org/manual/5.4/manual.html#6.4.1)
- [Lrexlib](https://github.com/rrthomas/lrexlib): Uma coleção de binding para diferentes bibliotecas de expressões regulares.
- [LPeg](http://www.inf.puc-rio.br/~roberto/lpeg/): Uma biblioteca para análise de padrões (patterns) com gramáticas de Parsing Expression.
- [Lua-users wiki: Patterns Tutorial](http://lua-users.org/wiki/PatternsTutorial): Um tutorial sobre o uso de padrões em Lua.