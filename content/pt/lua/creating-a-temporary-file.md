---
title:                "Criando um arquivo temporário."
html_title:           "Lua: Criando um arquivo temporário."
simple_title:         "Criando um arquivo temporário."
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e Por Que?

Criar um arquivo temporário significa criar um arquivo temporário que será apagado após o uso. Programadores geralmente criam arquivos temporários para armazenar informações temporárias que não são necessárias permanentemente e também para evitar sobrecarga no armazenamento.

## Como Fazer:

Exemplo de código:

```lua
-- forma mais básica de criar um arquivo temporário
arquivo_temp = io.tmpfile()

-- escrevendo em um arquivo temporário
arquivo_temp:write("Este é um arquivo temporário!")

-- lendo o conteúdo do arquivo temporário
arquivo_temp:seek("set")
print(arquivo_temp:read("*a"))

-- fechando e apagando o arquivo temporário
arquivo_temp:close()
```

Saída do código:

```
Este é um arquivo temporário!
```

## Mergulho Profundo:

Os arquivos temporários existem desde os primórdios da programação e são uma prática comum para lidar com informações temporárias. Existem alternativas para criar arquivos temporários, como usar a função `os.tmpname()` ou criar um arquivo regular e usá-lo como arquivo temporário. O Lua possui uma biblioteca padrão `io` que fornece métodos para criar, ler e escrever em arquivos temporários.

## Veja Também:

- [Documentação do Lua sobre arquivos temporários](https://www.lua.org/manual/5.3/manual.html#pdf-io.tmpfile)
- [Alternativa para criar arquivos temporários em Lua](http://lua-users.org/wiki/TempFileNames)
- [Exemplos práticos de uso de arquivos temporários em Lua](https://www.tutorialspoint.com/lua/lua_io_files.htm)