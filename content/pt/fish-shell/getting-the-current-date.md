---
title:                "Fish Shell: Obtendo a data atual"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que utilizar Fish Shell para obter a data atual?

Fish Shell é um poderoso shell de linha de comando com muitos recursos úteis para os programadores. Entre esses recursos, está a capacidade de obter a data atual de forma fácil e rápida. Se você é um programador que precisa utilizar a data em seus projetos, continue lendo para aprender como fazer isso com Fish Shell!

## Como obter a data atual utilizando Fish Shell?

Para obter a data atual utilizando Fish Shell, basta utilizar o comando `date` seguido de `%Y-%m-%d` para especificar o formato da data. Veja um exemplo abaixo:

```Fish Shell
date +%Y-%m-%d
```

Este comando irá imprimir a data atual no formato ano-mês-dia, como por exemplo `2021-01-22`.

## Mergulho profundo

Se você quiser mergulhar ainda mais no assunto, você pode utilizar o comando `man date` para ver todas as opções disponíveis para formatar a data. Além disso, você pode utilizar o `date` com outros comandos Fish Shell, como por exemplo `echo` para imprimir a data em uma frase ou `touch` para criar um arquivo com o nome da data atual.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial do Fish Shell para iniciantes](https://opensource.com/article/20/6/fish-shell)
- [Comandos úteis do Fish Shell](https://github.com/jorgebucaran/awesome-fish)