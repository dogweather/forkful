---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O quê e Por quê?

Argumentos de linha de comando são entradas fornecidas ao programa durante sua execução no terminal. Os programadores os usam para personalizar a execução de um programa, permitindo que o mesmo código se comporte de maneiras diferentes dependendo de suas entradas.

## Como fazer:

Em Fish Shell, você lê argumentos da linha de comando usando a variável especial `$argv`. Veja um exemplo simples:

```Fish Shell
for arg in $argv
    echo "Argumento: $arg"
end
```

Executando este script com `./meuscript.fish arg1 arg2 arg3`, você veria:

```Fish Shell
Argumento: arg1
Argumento: arg2
Argumento: arg3
```

## Mergulho Profundo

O Fish Shell é relativamente novo na cena do shell, lançado em 2005, mas desde então se tornou popular devido à sua sintaxe fácil de entender e excelente suporte para script. Embora outras shells como bash e zsh também possuam recursos para ler argumentos de linha de comando, o Fish é especialmente apreciado por sua clareza.

Alternativas para ler argumentos incluem o uso de flags e a sintaxe `getopts`, proporcionando maior controle sobre a entrada, mas esses métodos podem ser mais complexos.

O Fish implementa a leitura de argumentos como um array indexado de `$argv`, começando do 1. Esta é uma diferença chave das linguagens de programação C-like onde o array geralmente começa no 0. Esteja ciente disso ao escrever seus scripts.

## Veja Também

1. [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
2. [Guia de argumentos de linha de comando do Fish Shell](https://fishshell.com/docs/current/commands.html#arguments)