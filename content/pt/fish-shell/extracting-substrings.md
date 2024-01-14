---
title:                "Fish Shell: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair substrings pode ser útil para usuários do Fish Shell que querem manipular e trabalhar com parte específicas de uma string. Com o uso dessa funcionalidade, é possível automatizar tarefas que envolvem o manuseio de informações de texto, tornando o trabalho mais eficiente e fácil.

## Como Fazer

Extrair substrings no Fish Shell é bastante simples. Primeiramente, é necessário ter uma string que contenha a informação desejada. Esta string pode ser armazenada em uma variável ou inserida diretamente no comando. Em seguida, utilizamos o comando `substr` seguido de um intervalo de índices. Por exemplo:

```
set string "Hello World!"
echo (string (substr $string 0 5))
```

Neste exemplo, o comando `substr` extrairá os cinco primeiros caracteres da string e o comando `echo` exibirá o resultado no terminal, mostrando "Hello".

Também é possível utilizar operadores de comparação nos intervalos de índices, como no exemplo abaixo:

```
set string "Hello World!"
echo (string (substr $string (- (count $string) 1) (count $string)))
```

Neste caso, o comando `substr` extrairá somente o último caractere da string e o comando `echo` exibirá o resultado, mostrando o ponto de exclamação.

## Deep Dive

O comando `substr` pode ser utilizado com diversos intervalos de índices, como em um laço `for` por exemplo. É importante lembrar que o primeiro índice é sempre 0 e o último índice é o total de caracteres na string menos 1, já que os caracteres são contados a partir do 0.

Também é possível utilizar uma variável no lugar do índice final, permitindo flexibilidade e dinamismo no código. Por exemplo:

```
set beginning 3
set end 7
set string "Hello World!"
echo (string (substr $string $beginning $end))
```

Neste exemplo, o comando `substr` extrairá a string "lo Wo" e o comando `echo` a exibirá no terminal.

## Veja Também

- [Documentação do Fish Shell sobre o comando `substr`](https://fishshell.com/docs/current/cmds/substr.html)
- [Tutorial sobre manipulação de strings no Fish Shell](https://blog.sanctum.geek.nz/manipulating-strings-using-fish-shell/)
- [Exemplos de uso do comando `substr`](https://fishshell.com/docs/current/tutorial.html#tut_substrings)