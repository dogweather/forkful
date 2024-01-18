---
title:                "Lendo um arquivo de texto"
html_title:           "Lua: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por quê?
Ler um arquivo de texto é um processo comum e essencial na programação, que envolve a leitura e acesso às informações contidas em um arquivo de texto. Programadores frequentemente utilizam essa ação para obter dados armazenados em um arquivo, como configurações, listas, textos, entre outros.

## Como fazer:
```
-- Abrindo e lendo um arquivo de texto
arquivo = io.open("arquivo.txt", "r")
conteudo = arquivo:read("*all")
arquivo:close()

-- Imprimindo o conteúdo do arquivo
print(conteudo)
```
**Saída:**
```
Este é o conteúdo do arquivo de texto.
```

## Imersão Profunda:
Ler arquivos de texto remonta aos primeiros dias da programação, quando os computadores eram incapazes de armazenar grandes quantidades de dados na memória. Alternativas como bancos de dados e APIs com suporte a leitura de arquivos também existem, mas ler um arquivo de texto ainda é uma opção simples e confiável. A leitura de arquivos também pode ser combinada com outras operações, como escrita e edição de arquivos.

## Veja Também:
- [Guia Oficial Lua para Manipulação de Arquivos](https://www.lua.org/pil/21.html)
- [Documentação Lua sobre Abertura de Arquivos](https://www.lua.org/manual/5.3/manual.html#6.8)