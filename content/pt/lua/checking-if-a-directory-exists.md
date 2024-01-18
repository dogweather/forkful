---
title:                "Verificando se um diretório existe."
html_title:           "Lua: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que é e Por Quê?

Verificar se um diretório existe é uma tarefa comum para programadores em Lua. Essencialmente, esta é uma verificação para determinar se determinado caminho em um sistema de arquivos é válido e pode ser acessado. Programadores fazem esta verificação para melhorar a segurança do seu código e garantir que as operações em arquivos sejam realizadas corretamente.

## Como fazer:

Para verificar se um diretório existe em Lua, basta utilizar a função `lfs.attributes(path)` do módulo LuaFileSystem. Esta função retorna uma tabela com informações sobre o caminho especificado, incluindo se o diretório existe ou não. Veja o exemplo abaixo:

```Lua
local lfs = require("lfs") -- importa o módulo LuaFileSystem

-- define o caminho a ser verificado
local path = "/home/diretorio/exemplo/"

-- chama a função lfs.attributes e armazena o resultado na variável 'info'
local info = lfs.attributes(path)

-- verifica o valor da chave 'mode' na tabela de informações
-- se for igual a "directory", significa que o diretório existe
if info.mode == "directory" then
    print("O diretório existe!")
else
    print("O diretório não existe.")
end
```

Saída:

```
O diretório existe!
```

## Mergulho Profundo:

A função `lfs.attributes` foi introduzida em LuaFileSystem versão 1.2.0 e suporta tanto a verificação de diretórios quanto de arquivos. No entanto, para a verificação de diretórios, é recomendado utilizar a função `lfs.isdir(path)`, que é mais direta e retorna um booleano indicando se o diretório existe ou não. Outra opção é utilizar a função `os.execute(command)` para executar um comando do sistema operacional que verifica se o diretório existe. Porém, esta abordagem é menos eficiente e pode ter problemas de segurança.

## Veja Também:

- Documentação oficial do LuaFileSystem: http://keplerproject.github.io/luafilesystem/
- Função `lfs.isdir()`: http://luaforge.net/docman/83/98/ANiceSolution-1.2-svn/coverage/lfs/bind.c.html#lfs.isdir
- Exemplos da função `os.execute()`: https://gist.github.com/NaftuliTzvi/d5d204abdb7ecfcac291