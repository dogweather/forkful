---
title:                "Convertendo uma data em uma sequência de caracteres"
html_title:           "Lua: Convertendo uma data em uma sequência de caracteres"
simple_title:         "Convertendo uma data em uma sequência de caracteres"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Converter uma data em uma string é uma tarefa comum na programação, em que uma data é transformada em uma representação visual legível para humanos. Isso é necessário para que as datas possam ser exibidas e manipuladas de forma mais intuitiva.

Os programadores geralmente fazem isso porque a linguagem de programação pode ter uma forma de armazenar datas internamente, mas é mais conveniente exibi-las em uma forma mais legível para os usuários.

## Como fazer:

```Lua
-- Primeiro, precisamos importar a biblioteca padrão de datas do Lua:
local date = require "date"

-- Em seguida, criamos uma instância da data que queremos converter:
local minhaData = date(2020, 05, 20)

-- Agora, usamos o método tostring () para converter a data em uma string:
print(tostring(minhaData))
-- Output: "2020-05-20"
```

## Mergulho profundo:

Historicamente, a conversão de datas em strings costumava ser feita manualmente, usando operações aritméticas para obter o dia, mês e ano e depois juntá-los em uma string no formato desejado. Felizmente, com o avanço da tecnologia, a maioria das linguagens de programação agora possui métodos embutidos ou bibliotecas específicas para converter datas em strings.

Alternativamente, os programadores também podem optar por manipular as datas em um formato de timestamp, que é uma representação numérica do tempo decorrido desde uma data específica. Isso pode ser útil para cálculos mais complexos com datas, mas não é tão intuitivo para os usuários finais.

No Lua, a biblioteca "date" suporta uma variedade de formatos de data e hora, incluindo RFC 3339 e ISO 8601. Além disso, os usuários podem personalizar a representação de datas usando o método newformat () e especificando o formato desejado.

## Veja também:

- Documentação oficial da biblioteca "date" do Lua: https://keplerproject.github.io/date/

- Tutorial sobre como lidar com datas em Lua: https://www.lua.org/pil/22.1.html

- Guia abrangente sobre o padrão ISO 8601 para formatação de datas: https://www.iso.org/files/live/sites/isoorg/files/archive/pdf/en/iso8601-2004e.pdf