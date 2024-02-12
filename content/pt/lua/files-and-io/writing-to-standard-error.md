---
title:                "Escrevendo para o erro padrão"
aliases:
- /pt/lua/writing-to-standard-error.md
date:                  2024-02-03T19:33:41.522942-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Escrever no erro padrão (stderr) trata de direcionar mensagens de erro e saídas de diagnóstico para um canal separado, distinto da saída padrão (stdout). Programadores fazem isso para diferenciar resultados regulares do programa de informações de erro, otimizando processos de depuração e registro.

## Como fazer:
Em Lua, escrever no stderr pode ser alcançado utilizando a função `io.stderr:write()`. Aqui está como você pode escrever uma simples mensagem de erro para o erro padrão:

```lua
io.stderr:write("Erro: Entrada inválida.\n")
```

Caso precise sair uma variável ou combinar múltiplos pedaços de dados, concatene-os dentro da função de escrita:

```lua
local mensagemDeErro = "Entrada inválida."
io.stderr:write("Erro: " .. mensagemDeErro .. "\n")
```

**Exemplo de Saída no stderr:**
```
Erro: Entrada inválida.
```

Para cenários mais complexos, ou quando trabalhando com aplicações maiores, você pode considerar bibliotecas de registro de terceiros, como LuaLogging. Com LuaLogging, você pode direcionar logs para diferentes destinos, incluindo stderr. Aqui está um breve exemplo:

Primeiro, certifique-se de que LuaLogging esteja instalado usando LuaRocks:

```
luarocks install lualogging
```

Então, para escrever uma mensagem de erro no stderr usando LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Erro: Entrada inválida.")
```

Essa abordagem oferece a vantagem de um registro padronizado em sua aplicação, com a flexibilidade adicional de definir níveis de log (por exemplo, ERROR, WARN, INFO) através de uma API simples.
