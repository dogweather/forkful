---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:37:26.969888-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
"Parsing" de data refere-se ao processo de converter uma string que contém informação de data e hora em uma estrutura de dados que um programa possa entender e processar. Programadores fazem isso porque as datas são geralmente armazenadas e manipuladas em formatos diferentes, sendo necessário um padrão para trabalharmos com elas de maneira mais fácil e eficiente.

## Como Fazer:
```Lua
-- Vamos começar com uma string de data
local data_string = "25/03/2023 14:20:00"

-- Dividindo a string para extrair os componentes da data
local dia, mes, ano, hora, minuto, segundo = data_string:match("(%d+)/(%d+)/(%d+) (%d+):(%d+):(%d+)")

-- Convertendo as strings extraídas para números
dia, mes, ano, hora, minuto, segundo = tonumber(dia), tonumber(mes), tonumber(ano), tonumber(hora), tonumber(minuto), tonumber(segundo)

-- Criando uma tabela os.date com os componentes da data
local data_tabela = os.time({year = ano, month = mes, day = dia, hour = hora, min = minuto, sec = segundo})

-- Exibindo a data no formato timestamp
print(data_tabela)
```
Saída de exemplo:
```
1679660400
```

## Mergulho Profundo:
O "parsing" de datas tem suas raízes na necessidade de normalizar e comparar timestamps ao longo da história da computação. Em Lua, não há um módulo padrão dedicado exclusivamente a parsing de datas, ao contrário de outras linguagens que possuem bibliotecas robustas para tal (como o DateTime em .NET ou moment.js em JavaScript).

Alternativas para parsing de datas em Lua, além da função 'os.time', incluem a biblioteca 'date' (que não é padrão e necessita ser instalada separadamente) ou o uso de expressões regulares para obter um controle mais detalhado sobre o parsing.

Detalhes de implementação importantes incluem o cuidado com os formatos de data, que variam entre regiões (por exemplo, MM/DD/AAAA versus DD/MM/AAAA) e a manipulação de casos como anos bissextos e fusos horários diferentes.

## Veja Também:
- Documentação oficial Lua 5.4: https://www.lua.org/manual/5.4/manual.html#6.9
- Tutorial Lua sobre 'os.date' e 'os.time': https://www.tutorialspoint.com/lua/lua_dates_time.htm
- Lua Users Wiki sobre manipulação de datas: http://lua-users.org/wiki/DateTime
- Biblioteca 'date' para Lua: https://github.com/Tieske/date

Lembre-se que a prática leva à perfeição, então vá em frente e comece a brincar com datas e horários no Lua - é um ótimo exercício para fortalecer sua compreensão dos fundamentos da linguagem!
