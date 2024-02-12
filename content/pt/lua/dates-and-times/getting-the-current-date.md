---
title:                "Obtendo a data atual"
aliases:
- /pt/lua/getting-the-current-date.md
date:                  2024-02-03T19:10:19.695808-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obtendo a data atual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Recuperar a data atual na programação é uma tarefa crucial para uma infinidade de aplicações, incluindo registro de atividades (logging), marcação de eventos com carimbo de data/hora ou agendamento de tarefas. No Lua, essa funcionalidade permite que os programadores lidem com operações de data e hora de maneira integrada em suas aplicações, garantindo que o software possa interagir efetivamente com dados em tempo real.

## Como fazer:

O Lua fornece a função `os.date` para obter a data e a hora atuais. A função pode ser usada sem argumentos para obter uma string formatada ou com especificadores de formato para personalizar a saída. Veja como usá-la:

```lua
-- Obtendo a data e hora atuais como uma string formatada
print(os.date())  -- ex., Qui Mar  3 14:02:03 2022

-- Personalizando o formato de saída
-- %Y para ano, %m para mês, %d para dia, %H para hora, %M para minutos
print(os.date("%Y-%m-%d %H:%M"))  -- ex., 2022-03-03 14:02
```

Para manipulações mais sofisticadas de data e hora, o Lua não possui bibliotecas internas tão ricas quanto algumas outras linguagens de programação. No entanto, você pode usar bibliotecas de terceiros, como `lua-date` (https://github.com/Tieske/date). Esta biblioteca oferece funcionalidades mais abrangentes para manipular datas e horários. Veja como você pode usá-la:

Primeiro, certifique-se de ter instalado a biblioteca `lua-date`. Geralmente, você pode instalá-la usando o LuaRocks com o seguinte comando:

```bash
luarocks install lua-date
```

Então, você pode usá-la em seu script Lua assim:

```lua
local date = require("date")

-- Criando um objeto de data para a data e hora atuais
local agora = date()

print(agora:fmt("%Y-%m-%d %H:%M:%S"))  -- ex., 2022-03-03 14:02:03
```

Este exemplo demonstra a criação de um objeto `date` representando o momento atual, o qual você pode então formatar de maneira similar à função `os.date`, mas com flexibilidade e opções adicionais fornecidas pela biblioteca `lua-date`.
