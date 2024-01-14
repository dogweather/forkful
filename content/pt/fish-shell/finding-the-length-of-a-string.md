---
title:                "Fish Shell: Encontrando o comprimento de uma string"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum na programação, especialmente quando estamos lidando com strings de entrada de usuários ou manipulando texto em nossos scripts. Conhecer o comprimento de uma string pode nos ajudar a fazer validações de entrada ou realizar cortes precisos em nossos dados. Neste artigo, vamos descobrir como podemos encontrar facilmente o comprimento de uma string usando o Fish Shell.

## Como Fazer

Para encontrar o comprimento de uma string usando o Fish Shell, podemos utilizar o comando `string length`. Este comando retorna o número de caracteres em uma string. Vamos dar uma olhada em alguns exemplos para entender melhor seu funcionamento.

```Fish Shell
$ string length "Olá mundo"
10
$ string length "óculos 🕶️"
11
$ string length "12345"
5
```

Podemos ver que o comando `string length` funciona com qualquer tipo de string, seja com letras, números ou até mesmo emojis. Além disso, ele também conta os espaços em branco e caracteres especiais.

Outra forma de encontrar o comprimento de uma string é utilizando a função `count` em conjunto com o caractere coringa `.`. Isso nos permitirá contar quantas vezes o caractere se repete na string, o que, no caso de strings, será o número de caracteres.

```Fish Shell
$ count . "Olá mundo"
10
$ count . "óculos 🕶️"
11
$ count . "12345"
5
```

Agora que já sabemos como encontrar o comprimento de uma string, podemos utilizá-lo em nossos scripts para facilitar a manipulação de dados e validações de entrada.

## Mergulho Profundo

Em alguns casos, a função `count` pode ser mais eficiente do que o comando `string length`, pois ela não precisa percorrer toda a string, apenas contar as ocorrências de um caractere. Além disso, podemos passar um critério de busca como segundo argumento, o que possibilita contagem de caracteres específicos.

Outra dica é que o caractere `.` pode ser substituído por qualquer outro caractere ou sequência de caracteres, o que pode ser particularmente útil em situações específicas.

## Veja Também

- [Documentação do Fish Shell - Comando `string length`](https://fishshell.com/docs/current/cmds/string-length.html)
- [Documentação do Fish Shell - Função `count`](https://fishshell.com/docs/current/cmds/count.html)
- [Tutorial do Fish Shell (em português)](https://terminalroot.com.br/fish)