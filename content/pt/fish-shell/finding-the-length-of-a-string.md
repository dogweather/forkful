---
title:                "Fish Shell: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos saber o tamanho de uma string em nosso código para realizar operações específicas. O Fish Shell oferece uma maneira simples e eficaz de encontrar o comprimento de uma string usando alguns comandos básicos.

## Como fazer

Para encontrar o comprimento de uma string no Fish Shell, você pode usar o comando `count`. Basta digitar o comando seguido da string entre aspas para obter o resultado. Veja um exemplo abaixo:

```
Fish Shell
count "Olá, mundo!"

12
```

Neste exemplo, a string dentro das aspas tem 12 caracteres, incluindo espaços e pontuações.

Você também pode usar o recurso de expansão de variáveis do Fish Shell para encontrar o comprimento de uma variável. Vamos ver um exemplo:

```
set mensagem "Esta é uma mensagem"
count $mensagem

23
```

Observe que usamos o cifrão antes do nome da variável para indicar ao Fish Shell que queremos expandi-la antes de usar o comando `count`.

## Mergulho profundo

O Fish Shell possui uma função embutida chamada `string length`, que também pode ser usada para encontrar o comprimento de uma string. Essa função é particularmente útil se você deseja armazenar o resultado em uma variável para uso posterior.

Veja um exemplo:

```
set comprimento (string length "Fish Shell")

11
```

Observe que agora nós armazenamos o resultado em uma variável chamada "comprimento" usando parênteses e sem precisar usar o cifrão antes da variável.

## Veja também

Aqui estão algumas referências úteis sobre como encontrar o comprimento de uma string no Fish Shell:

- [Documentação oficial do Fish Shell sobre o comando `count`](https://fishshell.com/docs/current/cmds/count.html)
- [Dica rápida: Encontrando o comprimento de uma string no Fish Shell](https://dev.to/thebitflare/quick-tip-finding-the-length-of-a-string-in-fish-shell-1g6o)
- [Vídeo tutorial do Fish Shell sobre o comando `string length`](https://www.youtube.com/watch?v=fZOfThb0Y0w)