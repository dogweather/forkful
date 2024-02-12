---
title:                "Verificando se um diretório existe"
aliases:
- /pt/lua/checking-if-a-directory-exists/
date:                  2024-02-03T19:07:54.411229-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Verificar se um diretório existe é uma operação fundamental ao escrever scripts que interagem com o sistema de arquivos, garantindo que seu programa opere em caminhos válidos e previna erros relacionados a diretórios inexistentes. Essa tarefa é crucial para criar novos arquivos em diretórios, ler a partir deles, ou executar operações específicas de diretório de forma segura.

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
