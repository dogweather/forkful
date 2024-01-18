---
title:                "Obtendo a data atual"
html_title:           "Lua: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

Obter a data atual é um processo importante para programadores, pois permite que eles acompanhem o tempo e a data em que o código foi executado. Isso pode ser útil para muitas coisas, como rastreamento de logs, agendamento de tarefas e registro de timestamps em bancos de dados.

## Como fazer:

Para obter a data atual em Lua, podemos usar a função `os.date()`. Esta função aceita dois argumentos: o primeiro é um padrão de formato que determina como a data será exibida, e o segundo é o timestamp em segundos, que se não for fornecido será o timestamp atual.

```Lua
-- Exemplo 1: Obtendo a data atual no formato padrão
local current_date = os.date()
print(current_date) -- output: Sat Jul 10 22:50:50 2021

-- Exemplo 2: Obtendo a data atual no formato "ano/mês/dia"
local current_date = os.date("%Y/%m/%d")
print(current_date) -- output: 2021/07/10
```

## Mergulho profundo:

Historicamente, a contagem de tempo em computadores era feita a partir de um momento conhecido como "época", que representa o marco zero do tempo. Em sistemas baseados em Unix, a época é 1º de janeiro de 1970 às 00:00:00 UTC. A função `os.date()` pode ser usada para converter timestamps em outros formatos de tempo, como horários locais ou no formato Unix Epoch.

Além disso, é importante mencionar que há outras formas de obter a data atual em Lua, como usando bibliotecas de terceiros, como o módulo `luautf8` ou `luaposix`.

## Veja também:

- [Documentação oficial do Lua sobre a função os.date()](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Módulo luaposix](https://github.com/rmbhorton/luaposix)
- [Módulo luautf8](https://github.com/starwing/luautf8)