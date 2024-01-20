---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Encontrar o tamanho de uma string significa determinar o número de caracteres nela. Programadores precisam disso para manipular e organizar informações de forma eficaz.

## Como fazer:

Aqui está um exemplo simples de como encontrar o tamanho de uma string no Fish Shell.

```Fish Shell
set frase "olá, mundo"
echo (string length "$frase")
```

A saída seria `11`, a quantidade de caracteres na string incluindo espaços e pontuação.

## Mergulho Profundo

Historicamente, programadores têm determinado o tamanho das strings de várias maneiras, cada linguagem de programação tem sua própria abordagem. Em Fish Shell, a função `string length` fornece um meio simples e eficaz.

Há outras maneiras de obter o mesmo resultado. Por exemplo, você pode usar uma função de loop para iterar por cada caractere da string e incrementar um contador. No entanto, esta é uma forma menos eficiente e ótima.

A implementação da função `string length` é baseada na capacidade do interpretador Fish Shell de manipular diretamente cadeias de caracteres Unicode, uma característica que a torna atraente em relação a outras shells.

## Veja também

Para mais referências sobre o trabalho com strings no Fish Shell, confira estes recursos:

- [Documentação Oficial do Fish Shell String](https://fishshell.com/docs/current/cmds/string.html)
- [Treinamento de Shell Scripting](https://www.shellscript.sh/)