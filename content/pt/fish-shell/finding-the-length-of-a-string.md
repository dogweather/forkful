---
title:    "Fish Shell: Encontrando o comprimento de uma sequência de caracteres"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Porquê

Encontrar o tamanho de uma string é uma tarefa comum no desenvolvimento de aplicações. Saber como fazer isso pode economizar tempo e evitar erros de código desnecessários. Além disso, é uma habilidade básica que todo programador deve dominar.

## Como Fazer

Usando o Fish Shell, é fácil encontrar o tamanho de uma string usando a função `string length`seguida da string desejada entre parênteses. Veja um exemplo:

```
Fish Shell string length example
echo (string length "Hello world")

```

O resultado deste código será 11, pois a string "Hello world" tem 11 caracteres. É importante notar que, enquanto a string entre aspas é obrigatória, os parênteses podem ser omitidos se a string for a única entrada para a função.

```
Fish Shell string length example
echo (string length "I am learning Fish Shell!")

```

O resultado deste código será 26, pois a string tem 26 caracteres.

## Deep Dive

Ao usar a função `string length`, é possível perceber que ela não conta apenas letras, mas também espaços, números e caracteres especiais. Além disso, se a string contiver acentos ou caracteres Unicode, eles também serão contados no resultado.

No entanto, é importante notar que, dependendo da codificação de caracteres utilizada, um caractere Unicode pode ser contado como mais de um caractere. Isso pode afetar o comprimento total da string e deve ser considerado ao usar a função `string length`.

## Veja também

- Documentação oficial do Fish Shell sobre a função `string length`: https://fishshell.com/docs/current/cmds/string_length.html
- Tutorial sobre como usar o Fish Shell: https://fishshell.com/docs/current/tutorial.html
- Lista de funções do Fish Shell: https://fishshell.com/docs/current/commands.html