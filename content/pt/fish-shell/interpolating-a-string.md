---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

Título: Interpolação de Strings com o Fish Shell

## O Que e Por Que?

A interpolação de strings é a incorporação de variáveis dentro de uma string. Programadores fazem isso para tornar o código mais limpo e menos propenso a erros.

## Como Fazer:

Você pode usar aspas simples para strings não interpoladas (sem variáveis), e aspas duplas para strings interpoladas. Veja abaixo:
```Fish Shell
set nome "Dev"
echo "Olá, $nome"  # Olá, Dev
echo 'Olá, $nome'  # Olá, $nome
```
E se a variável não estiver definida?
```Fish Shell
echo "Olá, $sobrenome"  # Olá,
```
Nenhum erro é disparado. O valor não definido simplesmente desaparece.

## Aprofundando:

Interpolação de strings é uma ideia antiga que vem de linguagens mais antigas como Perl e Ruby. As alternativas em Shell incluem a concatenação de strings ou o uso de `printf`, mas a interpolação tende a ser mais legível.

Internamente, o Fish substitui a variável pela string logo após o analisador identificar que ele está dentro de aspas duplas. Isso é antes da execução dos comandos.

## Veja Mais:

Dive in to the wonderful world of Fish Shell with the following resources:

- Documentação oficial do Fish Shell: [link](https://fishshell.com/docs/current/index.html)
- Como trabalhar com Strings no Fish: [link](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- Guia de introdução ao Fish Shell: [link](https://fishshell.com/docs/current/tutorial.html)