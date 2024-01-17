---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e Por que?

Converter uma string para letras minúsculas é uma maneira de padronizar o texto em um programa de computador. Os programadores fazem isso porque torna mais fácil comparar e manipular strings, independentemente do caso das letras.

## Como fazer:

Usando a linguagem de programação Fish Shell, você pode converter uma string para letras minúsculas usando o comando "string tolower". Por exemplo:

```
Fish Shell >>> string tolower "EXEMPLO" 
------------
exemplo
```

## Mergulho Profundo:

Historicamente, converter strings para letras minúsculas era necessário para tornar a linguagem do programa mais consistente, já que as letras maiúsculas e minúsculas eram originalmente tratadas como caracteres distintos. Hoje em dia, há várias outras maneiras de realizar essa tarefa, dependendo da linguagem de programação que você está usando. No Fish Shell, você também pode usar a função "string match -r", que converte automaticamente qualquer string para letras minúsculas antes de compará-las.

## Veja Também:

- Documentação oficial do comando string tolower no Fish Shell: https://fishshell.com/docs/3.3/cmds/string.html#string-tolower
- Outros métodos para converter strings para letras minúsculas em diferentes linguagens de programação: https://www.codewars.com/kata/5390bac347d09b7da40006f6