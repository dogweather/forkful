---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparando Duas Datas em Lua

## O Que & Por Que?

A comparação de duas datas envolve a verificação de qual data é anterior ou posterior à outra. Isto é essencial para programadores ao manipular informações temporais, como programar eventos futuros ou calcular a diferença entre datas.

## Como Fazer:

Para realizar a comparação de duas datas em Lua, faça um simples código:

```Lua
local dt1 = os.time{year=2022, month=12, day=1, hour=0}
local dt2 = os.time{year=2021, month=12, day=1, hour=0}

if dt1 > dt2 then
  print("Data 1 é posterior à Data 2")
else
  print("Data 2 é posterior à Data 1")
end
```
Resultado esperado:
```
Data 1 é posterior à Data 2
```

## Mergulhando Fundo

Lua, uma linguagem de script de alto nível, usa a função `os.time()` para representar a data e hora. A função `os.time()` também fornece a funcionalidade para comparar duas datas.

Algumas alternativas para comparar datas passam pelo uso de módulos de tempo externos, como o LuaDate ou o Luatz. No entanto, `os.time()` é mais leve e adequado para a maioria das situações.

Os detalhes de implementação são diretos – converta as datas em segundos desde a época (1 de Janeiro de 1970) e então compare esses valores. A data posterior resultará em um valor de tempo maior.

## Veja Também:

Para aprofundar em mais detalhes e utilizar funcionalidades mais avançadas, é possível consultar as seguintes fontes:

1. [Documentação Oficial Lua – os library](https://www.lua.org/manual/5.4/manual.html#6.9)
2. [LuaDate – Trabalhando com Datas](https://github.com/Tieske/date)
3. [LuaUsers – Manipulações de Datas](http://lua-users.org/wiki/DateAndTime)

Observe que a comunicação com externos Lua módulos, muitas vezes requer a instalação e a importação adequadas desses pacotes.