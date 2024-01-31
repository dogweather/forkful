---
title:                "Trabalhando com CSV"
date:                  2024-01-19
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV, ou "valores separados por vírgula", é um formato de arquivo usado para armazenar dados tabulares. Programadores utilizam CSV pela facilidade de importação e exportação de dados em várias linguagens e plataformas.

## How to:
Para manipular arquivos CSV em Lua, você pode ler linha por linha e usar a função `string.gmatch` para iterar pelos valores separados por vírgula.

```Lua
-- Lendo um arquivo CSV em Lua
local arquivo = io.open("dados.csv", "r")

for linha in arquivo:lines() do
    for valor in string.gmatch(linha, '([^,]+)') do
        print(valor)
    end
end

arquivo:close()
```

Saída de exemplo:
```
Nome
Idade
Cidade
Alice
30
São Paulo
Bob
25
Rio de Janeiro
```

## Deep Dive
Arquivos CSV têm sido utilizados desde o início dos anos 70, proporcionando uma forma simples de representar tabelas de dados antes do advento de sistemas de banco de dados modernos. Alternativas incluem JSON e XML, que oferecem estruturas mais complexas e metadados. A implementação de leitura de CSV em Lua é facilitada devido às poderosas funções de manipulação de strings do idioma; no entanto, casos com formatos mais complexos de CSV podem exigir bibliotecas especializadas.

## See Also
Para mais informações sobre manipulação de CSV em Lua e bibliotecas disponíveis, confira:

- The Lua Users Wiki CSV: http://lua-users.org/wiki/CommaSeparatedValues
- A biblioteca `LuaCSV`: https://github.com/geoffleyland/lua-csv
- Documentação de Lua `string.gmatch`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gmatch

Ao explorar esses recursos, você poderá trabalhar com CSVs de forma mais eficiente e expandir suas habilidades em Lua.
