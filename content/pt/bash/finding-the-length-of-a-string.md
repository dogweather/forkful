---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Encontrar o comprimento de uma string significa determinar o número de caracteres que ela possui. Os programadores fazem isso para manipular e controlar melhor o conteúdo das strings, o que é crucial em muitas aplicações de programação.

## Como fazer:

Bash torna mais fácil encontrar o comprimento de uma string. Veja como você pode fazer isso:

```Bash
string="Olá, Mundo!"
echo ${#string}
```

Neste exemplo, `${#string}` retorna o comprimento da string. A saída será:

```Bash
12
```

O resultado é 12 porque a string "Olá, Mundo!" contém 12 caracteres.

## Mergulho Profundo:

Historicamente, a funcionalidade para encontrar o comprimento de uma string foi introduzida no Bash 2.0. É uma maneira direta e eficiente de manipular strings.

Sobre as alternativas, alguns idiomas, como Python ou JavaScript, possuem funções integradas, como len() ou .length para calcular o comprimento de uma string. No entanto, Bash alcança o mesmo efeito com menos código, tornando-o uma ferramenta ideal para operações de strings na linha de comando.

O detalhe de implementação é que a sintaxe `${#string}` inicia substituindo o conteúdo da variável string e calculando seu comprimento. Esta funcionalidade é implementada na própria shell Bash, o que a torna mais eficiente em comparação a outros métodos.

## Veja Também:

Para mais informações sobre manipulação de strings no Bash, confira os recursos a seguir:

- Guia de Programação Bash Avançada: https://tldp.org/LDP/abs/html/string-manipulation.html
- Manipulação de Strings Bash: https://www.thegeekstuff.com/2010/07/bash-string-manipulation
- Exemplos de Manipulação de Strings Bash: https://linuxize.com/post/bash-string-manipulation
- Guia de Referência do Bash: http://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html