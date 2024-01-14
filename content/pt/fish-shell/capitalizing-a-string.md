---
title:                "Fish Shell: Capitalizando uma string"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que utilizar o Fish Shell para capitalizar uma string?

Capitalizar uma string pode ser uma tarefa bastante comum ao trabalhar com dados ou strings de entrada. Ao utilizar o Fish Shell, podemos facilmente capitalizar strings com apenas algumas linhas de código, o que economiza tempo e esforço.

## Como fazer:

```Fish Shell
set texto "exemplo de string"
echo $texto | string capitalize
```

Este pequeno bloco de código demonstra como podemos capitalizar uma string utilizando o comando "string capitalize" no Fish Shell. Basta definir a string que desejamos capitalizar em uma variável e, em seguida, utilizar o comando "echo" junto com a variável e o comando "string capitalize".

O resultado do código acima será a seguinte saída:

```
Exemplo de string
```

Este é apenas um exemplo básico de como capitalizar uma string utilizando o Fish Shell. É importante notar que podemos usar esse comando com qualquer tipo de string, independentemente do tamanho ou quantidade de palavras.

## Aprofundando:

Ao utilizar o comando "string capitalize" no Fish Shell, além de capitalizar a primeira letra de cada palavra, também podemos especificar uma opção para capitalizar apenas a primeira letra de uma string. Isso pode ser útil ao trabalhar com títulos ou nomes próprios.

```Fish Shell
set texto "exemplo de string"
echo $texto | string capitalize -l
```

No código acima, a opção "-l" é utilizada para especificar que queremos capitalizar apenas a primeira letra da string. O resultado será:

```
Exemplo de string
```

Podemos, ainda, utilizar a opção "-a" para capitalizar todas as letras da string, ao invés de apenas a primeira letra de cada palavra.

```
set texto "exemplo de string"
echo $texto | string capitalize -a
```

A saída será:

```
Exemplo De String
```

## Veja também:

- [Documentação oficial do Fish Shell - String Manipulation](https://fishshell.com/docs/current/commands.html#string-manipulation)
- [Curso de Fish Shell para iniciantes](https://www.udemy.com/course/fish-shell-linux/)
- [Aprenda a usar o Fish Shell como um profissional](https://egghead.io/courses/preparing-for-the-fish-shell)