---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:54.411229-07:00
description: "Como fazer: No Lua, voc\xEA n\xE3o tem uma fun\xE7\xE3o embutida para\
  \ verificar diretamente se um diret\xF3rio existe, ent\xE3o frequentemente depende\
  \ da biblioteca Lua\u2026"
lastmod: '2024-03-13T22:44:46.723338-06:00'
model: gpt-4-0125-preview
summary: "No Lua, voc\xEA n\xE3o tem uma fun\xE7\xE3o embutida para verificar diretamente\
  \ se um diret\xF3rio existe, ent\xE3o frequentemente depende da biblioteca Lua File\
  \ System (lfs), uma biblioteca terceirizada popular para opera\xE7\xF5es de arquivo."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como fazer:
No Lua, você não tem uma função embutida para verificar diretamente se um diretório existe, então frequentemente depende da biblioteca Lua File System (lfs), uma biblioteca terceirizada popular para operações de arquivo.

Primeiro, garanta que você tenha Lua File System instalado. Caso contrário, geralmente você pode instalá-lo usando LuaRocks:

```sh
luarocks install luafilesystem
```

Então, você pode usar o seguinte exemplo para verificar a existência de um diretório:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- Verificar se um diretório específico existe
if directoryExists("/caminho/para/seu/diretório") then
    print("Diretório existe.")
else
    print("Diretório não existe.")
end
```

Isso vai sair:

```
Diretório existe.
```

Ou, se o diretório não existir:

```
Diretório não existe.
```

Essa abordagem usa a função `lfs.attributes` para obter os atributos do caminho. Se o caminho existir e seu atributo `mode` for `directory`, isso confirma a existência do diretório.
