---
title:                "Encontrando o comprimento de uma string"
html_title:           "Bash: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Encontrar o comprimento de uma string é uma tarefa comum em programação, especialmente em linguagens de script como o Bash. Isso ajuda os programadores a manipular e usar strings de forma eficiente e correta em seus códigos.

## Como fazer:
Para encontrar o comprimento de uma string em Bash, você pode usar o comando integrado ```${#var}```, onde "var" é a variável que contém a string que deseja medir. Por exemplo:

```
nome="João"
echo ${#nome}  # Output: 4
```
Você também pode usar o comando ```expr length $var```, que retornará o comprimento da string contida na variável "var". Por exemplo:
```
sobrenome="Silva"
expr length $sobrenome  # Output: 5
```

## Mergulho Profundo:
Encontrar o comprimento de uma string é uma tarefa básica em programação e tem sido usado desde os primórdios da computação. Em linguagens de programação mais antigas, como o C, é necessário declarar um tamanho fixo para uma string, o que torna a medida de sua comprimento mais crucial. Em Bash, o comando ```${#var}``` é a forma mais comum de obter o comprimento de uma string, mas também é possível usar o comando ```wc -c <<< "$var"``` ou até mesmo iterar por todos os caracteres da string usando um loop.

## Veja também:
- [Bash Guide para Iniciantes](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Bash Reference Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- [Bash One-Liners](https://www.linuxjournal.com/content/down-rabbit-hole-wildcards-and-bash-one-liners)