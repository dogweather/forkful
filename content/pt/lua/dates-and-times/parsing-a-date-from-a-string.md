---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:49.146099-07:00
description: "Analisar uma data a partir de uma string envolve converter representa\xE7\
  \xF5es textuais de datas e hor\xE1rios em um formato que pode ser facilmente manipulado,\u2026"
lastmod: 2024-02-19 22:05:05.770441
model: gpt-4-0125-preview
summary: "Analisar uma data a partir de uma string envolve converter representa\xE7\
  \xF5es textuais de datas e hor\xE1rios em um formato que pode ser facilmente manipulado,\u2026"
title: Analisando uma data a partir de uma string
---

{{< edit_this_page >}}

## O Que & Por Quê?
Analisar uma data a partir de uma string envolve converter representações textuais de datas e horários em um formato que pode ser facilmente manipulado, armazenado ou comparado dentro de um programa Lua. Programadores realizam essa tarefa para facilitar operações como agendamento, registro em log ou quaisquer cálculos temporais e para fechar a lacuna entre formatos de data legíveis por humanos e tipos de dados estruturados que um computador pode processar de maneira eficiente.

## Como Fazer:
Lua não possui suporte integrado para manipulação de datas e horários além da funcionalidade limitada fornecida pelas funções `os.date` e `os.time`. No entanto, estas podem ser aproveitadas para análises básicas, e para requisitos mais complexos, a biblioteca `luadate`, uma biblioteca externa, pode ser utilizada.

**Usando `os.date` e `os.time`:**
```lua
-- Converter uma data legível por humanos para um carimbo de data/hora e vice-versa
local dateString = "2023-09-21 15:00:00"
local padrão = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local ano, mês, dia, hora, minuto, segundo = dateString:match(padrão)

local carimboDeDataHora = os.time({
  ano = ano,
  mês = mês,
  dia = dia,
  hora = hora,
  min = minuto,
  sec = segundo
})

-- Converter carimbo de data/hora de volta para um formato legível por humanos
local dataFormatada = os.date("%Y-%m-%d %H:%M:%S", carimboDeDataHora)
print(dataFormatada)  -- Saída: 2023-09-21 15:00:00
```

**Usando `luadate` (biblioteca de terceiros):**
Para usar `luadate`, certifique-se de que está instalada via LuaRocks ou seu gerenciador de pacotes de preferência. `luadate` adiciona capacidades extensivas de análise e manipulação de datas e horas.

```lua
local date = require('date')

-- Analisar uma string de data diretamente
local dataAnalisada = date.parse("2023-09-21 15:00:00")
print(dataAnalisada:fmt("%Y-%m-%d %H:%M:%S"))  -- Saída: 2023-09-21 15:00:00

-- Adicionando durações
umaSemanaDepois = dataAnalisada:adddays(7)
print(umaSemanaDepois:fmt("%Y-%m-%d %H:%M:%S"))  -- Saída: 2023-09-28 15:00:00
```

A biblioteca `luadate` oferece uma maneira mais intuitiva e poderosa de trabalhar com datas, incluindo análise a partir de strings, formatação e operações aritméticas em datas, o que simplifica consideravelmente o trabalho com dados temporais em Lua.
