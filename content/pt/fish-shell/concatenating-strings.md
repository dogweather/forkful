---
title:                "Fish Shell: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings?

A concatenação de strings é uma tarefa muito útil em programação, pois permite que diferentes textos ou variáveis sejam combinados em uma única string. Isso é especialmente útil ao criar mensagens personalizadas ou ao gerar informações dinâmicas a partir de um conjunto de dados.

## Como fazer?

Para concatenar strings no Fish Shell, você precisa usar o operador `+` entre as diferentes partes da string que deseja unir. Por exemplo:

```Fish Shell
set saudacao "Olá"
set nome "João"

echo $saudacao" "$nome
```

Neste exemplo, a string "Olá" e o valor da variável `$nome` são concatenados usando o operador `+` para resultar em "Olá João" como saída.

Você também pode utilizar o operador `+` para unir mais de duas strings. Por exemplo:

```Fish Shell
set animal "gato"
set acao "miau"

echo $animal" diz "$acao
```

A saída desta concatenação seria "gato diz miau".

## Mergulho profundo

Ao concatenar strings, é importante ter em mente a ordem em que elas serão unidas. Em alguns casos, pode ser necessário usar parênteses para garantir que a ordem de concatenação seja correta.

Outro ponto importante é que o operador `+` só funciona com strings. Se você estiver tentando concatenar uma string com um número ou outra variável com um tipo de dados diferente, é provável que ocorra um erro.

Você também pode usar o comando `string` para converter diferentes tipos de dados em string antes de concatena-los. Por exemplo:

```Fish Shell
set numero 10
set texto "O valor do número é "

echo (string $numero)$texto
```

Com isso, a saída seria "O valor do número é 10".

## Veja também

- [Documentação oficial do Fish Shell para concatenação de strings](https://fishshell.com/docs/current/cmds/set.html#string-concatenation)
- [Tutorial sobre variáveis no Fish Shell](https://fishshell.com/docs/current/tutorial.html#variables)
- [Perguntas frequentes sobre o Fish Shell](https://fishshell.com/docs/current/faq.html)