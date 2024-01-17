---
title:                "Interpolação de uma string"
html_title:           "Fish Shell: Interpolação de uma string"
simple_title:         "Interpolação de uma string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Interpolar uma string é simplesmente inserir o valor de uma variável em uma string. Isso é útil para concatenação de strings e torna o código mais legível e conciso.

Os programadores costumam usar interpolação de strings para facilitar a manipulação de dados e tornar o código mais fácil de entender. Também economiza tempo de digitação, pois não há necessidade de concatenar várias strings manualmente.

## Como fazer:

```
Fish Shell:

# Exemplo 1:
set nome "João"
echo "Olá, $nome" # output: "Olá, João"

# Exemplo 2:
set numero 42
echo "A resposta para a vida, o universo e tudo mais é $numero" # output: "A resposta para a vida, o universo e tudo mais é 42"

```

## Aprofundando:

Embora a interpolação de strings seja uma prática comum em linguagens de programação, ela se originou na linguagem de template Perl. Alternativas para interpolar strings incluem concatenação manual com o sinal de adição (+) ou a função de formatação de strings (sprintf).

No Fish Shell, a interpolação de strings é realizada usando o caractere $ seguido pelo nome da variável a ser interpolada. Também é possível executar expressões aritméticas dentro de uma string interpolada, cercando a expressão com chaves ({}) para diferenciá-la do restante da string.

## Veja também:

Para mais informações sobre o uso de strings no Fish Shell, consulte a [documentação oficial do Fish Shell](https://fishshell.com/docs/current/tutorial.html#variables) e [outros recursos úteis](https://www.computerhope.com/unix/fish.htm).