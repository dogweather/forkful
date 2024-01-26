---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:15:30.506555-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Pegar a data atual é simplesmente saber qual é o dia de hoje segundo o computador. Programadores fazem isso para registrar eventos, comparar datas ou qualquer coisa que dependa de quando acontece.

## Como Fazer:
```Lua
-- Pegar a data e a hora atuais
local agora = os.date("*t") -- retorna uma tabela com a data atual

-- Exibir a data e hora no formato: "dd/mm/yyyy hh:mm:ss"
print(string.format("%02d/%02d/%02d %02d:%02d:%02d", 
      agora.day, agora.month, agora.year, agora.hour, agora.min, agora.sec))
```

Saída de exemplo:
```
05/04/2023 14:20:35
```

## Mergulho Profundo
Historicamente, Lua incorporou funções para manejar datas e tempos diretamente através da biblioteca `os`. Há uma função, `os.time()`, que te dá o tempo em segundos desde a Era Unix (o famoso "timestamp") e `os.date()`, que formata esse tempo de maneira legível para humanos. Ambas são herdadas do C, onde manipulação de tempo é tradicionalmente um tanto críptica.

Alternativas incluem usar bibliotecas de terceiros, como o `LuaDate` para mais funcionalidade. Porém, para aplicações simples, as funções nativas geralmente bastam.

Detalhes de implementação: `os.date("*t")` devolve uma tabela contendo todos os componentes da data e hora. Pode-se usar outros formatos de string como argumentos para `os.date()` para obter representações diferentes da data e hora.

## Veja Também
- [Documentação Oficial do Lua `os.date`](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- [Tutorial sobre datetime em Lua](http://lua-users.org/wiki/DateTime)
- [Repositório LuaDate no GitHub](https://github.com/Tieske/date)
