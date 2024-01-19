---
title:                "Verificando se um diretório existe"
html_title:           "Bash: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verificação de Diretórios no Lua: Um Guia Impresscindível

## O Que é e Porquê?

Verificar se um diretório existe é uma tarefa comum em programação, que implica em confirmar a existência de um diretório específico em um sistema de arquivos. Fazemos isto para evitar erros desconfortáveis durante a execução do código, como tentar acessar um diretório inexistente.

## Como Fazer:

O código a seguir mostra como você pode verificar a existência de um diretório usando Lua: 

```Lua
lfs = require('lfs')

function is_dir(path)
    -- Obtém o modo do arquivo. Retorna nil se o arquivo não existir
    local file_mode = lfs.attributes(path, "mode")
    -- Retorna verdadeiro se o modo for "directory", falso caso contrário
    return file_mode == "directory"
end

if is_dir("/caminho/para/diretório") then
    print("O diretório existe!")
else
    print("O diretório não existe!")
end
```

Se o diretório existir, você verá a mensagem "O diretório existe!". Caso contrário, você verá a mensagem "O diretório não existe!".

## Aprofundando no Assunto

A biblioteca de sistema de arquivos Lua (lfs) existe desde a versão 5.1 do Lua. Ela expande a funcionalidade do Lua para interagir com o sistema de arquivos e é usada para realizar verificações de existência de diretório.

Uma alternativa à lfs, que não requer bibliotecas externas, é usar a função `os.execute` com o comando `cd`. No entanto, este método não é recomendado, pois é menos eficiente e não é portátil entre diferentes sistemas operacionais.

Por último, é importante notar que apenas a verificação 'mode' é realizada. Isto significa que outros atributos do diretório, como permissões, não são considerados na função `is_dir`. Se você necessitar de uma verificação mais avançada, terá que expandir a função de acordo.

## Veja Também:

Para mais informações sobre lfs e funções relacionadas, consulte:
   - [LuaFileSystem Documentation](https://keplerproject.github.io/luafilesystem/)
   
Também recomendamos estudar sobre outras formas de interação com o sistema de arquivos no Lua:
   - [Programming in Lua: File System Operations](https://www.lua.org/pil/21.2.html)