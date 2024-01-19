---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Ler Arquivos de Texto com Bash: Guia Prático

## O Que & Por Quê?

Ler um arquivo de texto com Bash significa interpretar seu conteúdo linha por linha, usando código. É essencial para manipular dados, analisar logs, etc.

## Como Fazer

Aqui está um exemplo simples de como ler um arquivo de texto. 

```Bash
#!/bin/bash
while IFS= read -r line
do
  echo "$line"
done < "arquivo.txt"
```

Este script imprimirá cada linha do arquivo "arquivo.txt" no terminal. Substitua "arquivo.txt" pelo nome do arquivo que você deseja ler.

## Mergulho Profundo

### Contexto Histórico

Bash (Bourne Again SHell) foi criado em 1989 e revitalizou muitas funcionalidades de shells de Unix anteriores. A leitura de arquivos de texto é crucial desde os primeiros dias do Unix.

### Alternativas

Existem muitas outras ferramentas que podem ler arquivos de texto. Alguns exemplos não limitados ao universo Unix são: `more`, `cat`, `awk`, `perl`, Python e Ruby.

### Detalhes de Implementação

A linha `while IFS= read -r line` é usada para não perder nenhum espaço em branco no início ou fim das linhas. `IFS=` define a variável de campo interno (Internal Field Separator) para nada, e `-r` impede que `read` interprete a contrabarra como um caractere de escape.

## Veja Também

1. Manual do Bash: https://www.gnu.org/software/bash/manual/bash.html
2. Livro "Pro Bash Programming": https://www.amazon.com/dp/1484201221
3. Site StackOverflow: https://stackoverflow.com/questions/tagged/bash

Não há seção de conclusão.