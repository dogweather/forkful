---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever no erro padrão, `stderr`, separa mensagens de erro dos outputs normais. Isso ajuda a diagnosticar problemas sem interferir com o fluxo de saída de dados (`stdout`).

## How to:
Usa `io.stderr:write()` para mandar texto para o `stderr`. Exemplo:

```Lua
-- Escrever diretamente para stderr
io.stderr:write("Erro encontrado!\n")

-- Usar assert com mensagem de erro no stderr
local file, err = io.open("arquivo_inexistente.txt", "r")
if not file then
    io.stderr:write("Erro ao abrir arquivo: " .. err .. "\n")
end
```

Saída esperada no `stderr`:

```
Erro encontrado!
Erro ao abrir arquivo: arquivo_inexistente.txt: No such file or directory
```

## Deep Dive
O conceito de `stderr` vem dos sistemas Unix, onde `stdout` e `stderr` são dois streams de saída diferenciados. A vantagem é poder redirecionar erros para logs ou outros handlers enquanto a saída padrão segue inalterada. Em Lua, isso é menos evidente por ser uma linguagem de alto nível, mas a funcionalidade é fornecida por padrão. Alternativas incluem bibliotecas de logging que oferecem mais flexibilidade para manipular mensagens de erro.

## See Also
- Documentação do Lua `io` library: https://www.lua.org/manual/5.4/manual.html#6.8
- Tutorial de Lua sobre I/O: http://lua-users.org/wiki/IoLibraryTutorial
- Redirecionamento de stderr em sistemas Unix: https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)
