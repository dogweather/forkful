---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pesquisa e substituição no Fish Shell

## O que & Porquê?

'Pesquisa e substituição' é um método para trocar uma sequência de caracteres num texto por outro. Programadores fazem isso para corrigir bugs, melhorar a clareza do código ou atualizar funções obsoletas.

## Como fazer:

No Fish Shell, você pode usar o comando 'string replace':

``` Fish Shell
set frase "Araponga preta gosta de cantar"
echo $frase | string replace -r 'Araponga' 'Andorinha'
```

A saída fica assim:

``` Fish Shell
Andorinha preta gosta de cantar
```

## Mergulho Profundo:

###### Contexto histórico
'Pesquisa e substituição' são funções presentes nos editores de texto desde que foram inventados. No Fish Shell, eles simplificaram a sintaxe e combinaram as funcionalidades do 'grep' e 'sed' de shells Unix.

###### Alternativas
Se você está trabalhando com arquivos grandes ou precisa fazer substituições complexas, pode preferir o uso de 'grep', 'sed' ou 'awk'. Estes comandos do Unix são mais potentes, mas também mais difíceis de usar.
    
###### Detalhes de implementação
A substituição é feita lendo a entrada linha por linha. Isto significa que não vai funcionar se a sua string de pesquisa se espalha por várias linhas. 

## Ver também:

1. [Documentação do Fish Shell 'string replace'](https://fishshell.com/docs/current/cmds/string.html#string-replace)
2. [Tutorial de grep](https://ryanstutorials.net/linuxtutorial/grep.php)
3. [Introdução ao sed](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)
4. [Tutorial de awk](https://www.geekhideout.com/awk.shtml)