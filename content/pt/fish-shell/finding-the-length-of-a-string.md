---
title:                "Encontrando o comprimento de uma string"
html_title:           "Fish Shell: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Encontrar o comprimento (ou tamanho) de uma string é uma operação fundamental em programação. Isso porque, ao lidar com strings, é importante saber quantos caracteres ela contém para percorrer corretamente seus elementos, executar determinadas manipulações ou validar sua estrutura. Saber o comprimento de uma string também é útil para otimizar o uso de memória e garantir a eficiência do código.

## Como Fazer:

Usar o Fish Shell para encontrar o comprimento de uma string é simples e direto. Basta usar o comando `string length`, seguido da string entre aspas. Veja um exemplo abaixo:

```
Fish Shell
string length "Olá, mundo!" 
->
12
```

Também é possível usar variáveis contendo strings no lugar das aspas. Veja:

```
Fish Shell
set nome "João" 
string length $nome 
->
4
```

## Mergulho Profundo:

Encontrar o comprimento de uma string é uma tarefa muito comum em programação e, por isso, existem diversos métodos e funções para realizá-la em diferentes linguagens de programação. Alguns exemplos são o `strlen()` em C, o `len()` em Python e o `length()` em JavaScript.

É importante lembrar que, em algumas linguagens, o comprimento de uma string pode diferir do número de caracteres visíveis, já que contam com metacaracteres e acentuações, por exemplo. Portanto, é necessário conhecer bem a linguagem e suas características para utilizar corretamente os métodos de encontrar o comprimento de uma string.

## Veja Também:

- Documentação oficial do Fish Shell sobre `string length`: https://fishshell.com/docs/current/cmds/string_length.html
- Exemplos de uso do `string length` no Stack Overflow: https://stackoverflow.com/questions/38096876/how-to-find-the-length-of-a-string-in-fish-shell
- Tutorial sobre strings em programação: https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-javascript