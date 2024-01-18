---
title:                "Comparando duas datas"
html_title:           "Lua: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que e por que?

Comparação de duas datas é um processo comum em programação, onde duas datas são comparadas para determinar qual é a mais recente ou se elas são iguais. Os programadores geralmente fazem isso para lidar com lógica de data em seus programas, como verificar prazos de entrega ou datas de validade.

## Como fazer:

```Lua
-- Exemplo de comparação de datas
local data1 = os.time({ano=2020, mes=10, dia=15}) -- cria uma data específica
local data2 = os.time() -- pega a data atual
if data1 < data2 then -- comparação usando o operador "<"
    print("Data 1 é anterior a Data 2")
elseif data1 > data2 then -- comparação usando o operador ">"
    print("Data 1 é posterior a Data 2")
else -- comparção usando o operador "=="
    print("Datas são iguais")
end
```

Output:
```
Data 1 é anterior a Data 2
``` 

## Mergulho profundo:

A comparação de datas tem uma longa história na programação. Antes do advento de bibliotecas e funções especializadas, os programadores costumavam comparar as datas manualmente, convertendo-as em números e verificando digitos individuais. Felizmente, o Lua possui uma biblioteca de data padrão que facilita esse processo. Alternativamente, algumas linguagens possuem bibliotecas de terceiros para ajudar com essa tarefa. A comparação também pode ser feita com intervalos de tempo, ao invés de datas específicas.

## Veja também:

[Documentação da biblioteca de data do Lua](https://www.lua.org/manual/5.3/manual.html#6.9)