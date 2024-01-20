---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Leitura de Arquivos de Texto em Lua

## O Que & Por Quê?
A leitura de um arquivo de texto é o processo de acessar dados armazenados em um arquivo de texto pelo código do programa. Os programadores frequentemente precisam fazer isso para utilizar informações previamente registradas, como configurações de sistema ou bancos de dados pequenos.

## Como Fazer:
A leitura de um arquivo em Lua se faz em poucos passos. Veja:

#### Lendo todo o arquivo de uma vez:

```Lua
-- Abrir arquivo para leitura
local arquivo = io.open("meu_arquivo.txt", "r")

-- Ler todo o conteúdo do arquivo
local conteudo = arquivo:read("*a")

print(conteudo)  -- Exibir o conteúdo

-- Fechar o arquivo
arquivo:close()
```
#### Lendo o arquivo linha por linha:

```Lua
-- Abrir arquivo para leitura
local arquivo = io.open("meu_arquivo.txt", "r")

--Ler e imprimir cada linha do arquivo
for linha in arquivo:lines() do
    print(linha)
end

-- Fechar o arquivo
arquivo:close()
```

## Mergulho Profundo
A Lua, desde sua criação em 1993, tem uma biblioteca de E/S embutida, chamada io, que suporta leitura e gravação de arquivos.

Alternativamente, você pode usar a função `file:lines()` para obter um iterador que retorna uma nova linha a cada chamada. Ela é mais conveniente e menos propensa a erros comparada a usar `file:read()` em um loop.

Vale lembrar que é importante sempre fechar seus arquivos após a leitura para evitar vazamentos de memória.

## Veja Também
Se você deseja expandir seu entendimento sobre manipulação de arquivos em Lua, tente esses recursos:

- Documentação oficial do Lua: [Programming in Lua (PiL) - 21.1: File I/O](https://www.lua.org/pil/21.1.html)
- Livro Prático Lua: [Lua Users Wiki: Reading Files](http://lua-users.org/wiki/ReadingFiles)
- Tutoriais em vídeo: [YouTube: Lua - File I/O](https://www.youtube.com/watch?v=iMacxZQMPXs)