---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parseando uma data de uma string no Lua

## O que & Por quê? 

Parsear uma data de uma string consiste em extrair e convertê-la para um tipo data. Programadores fazem isso para manipular, comparar e calcular datas de forma mais eficiente.

## Como fazer:

No Lua, não existe uma função embutida para parsear datas, mas podemos criar a nossa própria função. Aqui está um exemplo:

```Lua
function parsearData(str)
    local y, m, d = str:match("(%d+)%-(%d+)%-(%d+)")
    return os.time({year=y, month=m, day=d})
end

dataStr = "2022-03-18"
data = parsearData(dataStr)

print(os.date("%x", data))
```

A saída deste código será a data em formatada:

```Lua
18/03/2022
```

## Mergulho profundo

No passado, a biblioteca `date.lua` era usada para manipular datas, contudo, ela não é nativa do Lua. É mais comum usar os recursos nativos do Lua para manipular datas.

Quanto às alternativas, existe uma biblioteca chamada `lua-date`: uma biblioteca leve e eficiente para parsear datas. A decisão entre criar a sua própria função ou usar uma biblioteca dependerá das necessidades específicas do seu projeto.

Além disso, é importante frisar que o Lua considera o mês a partir do índice 1 - Janeiro, ao contrário de outras linguagens que começam do 0 - Janeiro. Outro ponto notável é que a função `os.time()` retorna a data em segundos desde o Epoch (01/01/1970).

## Veja também

- Documentação do Lua: https://www.lua.org/manual/5.1/
- Github da biblioteca lua-date: https://github.com/tieske/date
- Tutorial mais avançado sobre datas no Lua: http://lua-users.org/wiki/DateAndTime
Nota: As bibliotecas de terceiros requerem a instalação separada.