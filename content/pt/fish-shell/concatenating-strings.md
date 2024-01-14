---
title:    "Fish Shell: Concatenando strings"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que usar a concatenação de strings no Fish Shell?

A concatenação de strings é uma ferramenta útil para juntar pedaços de texto em uma única string. Isso pode ser útil em várias situações, como criar nomes de arquivos, gerar mensagens personalizadas ou construir argumentos para comandos.

## Como fazer a concatenação de strings no Fish Shell

Para concatenar strings no Fish Shell, podemos usar o operador de adição `+` ou a função `string join`. Vamos ver alguns exemplos de como fazer isso.

```Fish Shell
set nome "Maria"
set sobrenome "Silva"
set nome_completo $nome" "$sobrenome
echo $nome_completo
```

Este código define duas variáveis, `nome` e `sobrenome`, e então usa o operador `+` para juntar as duas em uma única string `nome_completo`. Ao imprimir essa variável usando `echo`, obtemos a saída `Maria Silva`.

Outra forma de fazer a concatenação é usando a função `string join`. Vamos ver um exemplo:

```Fish Shell
set lista "banana" "maçã" "laranja"
set lista_concatenada (string join "; " $lista)
echo $lista_concatenada
```

Neste caso, usamos a função `string join` para juntar os itens da lista `lista` separados por `; `. A saída impressa será `banana; maçã; laranja`.

## Mergulho profundo na concatenação de strings

Na verdade, a concatenação de strings no Fish Shell é um pouco mais complexa do que simplesmente usar o operador `+` ou a função `string join`. Isso porque, por padrão, o Fish Shell utiliza uma ferramenta de prototype heredoc para lidar com strings, o que pode gerar comportamentos inesperados ao tentar concatenar strings contendo caracteres especiais. Para evitar esse problema, é recomendado usar a opção `-r` ao definir uma variável com uma string contendo caracteres especiais.

## Veja também

- [Documentação oficial do Fish Shell sobre concatenação de strings](https://fishshell.com/docs/current/cmds/set.html#set-string-command)
- [Expansão de variáveis no Fish Shell](https://fishshell.com/docs/current/index.html#variables)
- [Exemplos de strings contendo caracteres especiais no Fish Shell](https://fishshell.com/docs/current/cmds/set.html#set-string-command)