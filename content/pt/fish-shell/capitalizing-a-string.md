---
title:                "Fish Shell: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que usar o Fish Shell para capitalizar strings?

Capitalize strings é uma ação muito comum na programação, seja para padronizar a escrita de informações ou melhorar a usabilidade de um programa. No entanto, fazer isso manualmente pode ser demorado e suscetível a erros. É aí que o Fish Shell pode ser útil.

## Como capitalizar uma string usando o Fish Shell

É muito fácil capitalizar uma string usando o Fish Shell. Basta seguir os seguintes passos:

```Fish Shell
set texto "exemplo"  # Define a string que será capitalizada
echo $texto | string capitalize # Utiliza o comando "capitalize" do Fish Shell
```

O resultado será a string "Exemplo" impressa na tela. Caso queira salvar o novo valor da string na variável "texto", basta usar "set texto (string capitalize $texto)".

## Profundidade na capitalização de strings

O comando "string capitalize" do Fish Shell utiliza algumas regras simples para capitalizar uma string. Ele irá transformar a primeira letra em maiúscula e as restantes em minúsculas. No entanto, é importante notar que esse comando modifica a própria string, ou seja, ela não será mais igual à original.

Além disso, o Fish Shell possui outros comandos úteis para capitalizar strings, como "string capitalize-words" (que capitaliza cada palavra em uma string) e "string capitalize-sentence" (que capitaliza a primeira letra de cada frase em uma string).

## Veja também

- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Lista de comandos do Fish Shell](https://fishshell.com/docs/current/commands.html)
- [Exemplos de uso do comando "string capitalize"](https://fishshell.com/docs/current/cmds/string-capitalize.html)