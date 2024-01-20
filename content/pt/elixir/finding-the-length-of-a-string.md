---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encontrando o Tamanho de uma String em Elixir

## O Que & Por Quê?
Descobrir o tamanho de uma string significa achar o número total de caracteres nela. Em programação, isso é comum para validar entradas do usuário, limitar o tamanho da saída, entre outros.

## Como Fazer:
No Elixir, a função `String.length/1` nos ajuda a descobrir o tamanho de uma string. Veja o exemplo abaixo:

```Elixir
IO.puts String.length("Ola, mundo!")   
# Saída: 11
```
Nesse exemplo, a string `"Ola, mundo!"` tem 11 caracteres, incluindo a vírgula e o ponto de exclamação.

## Mergulho Profundo
Historicamente, em algumas linguagens de programação, encontrar o tamanho de uma string não é tão direto como em Elixir. Em C, por exemplo, você precisava iterar pelos caracteres da string até atingir o carácter de terminação nulo (ou seja, `'\0'`).

No Elixir, comparado a C, é mais intuitivo e menos propenso a erros. Usamos a função `String.length/1` da biblioteca padrão.

Note que `String.length/1` conta o número de grafemas e não o número de bytes. Um grafema geralmente corresponde a um caractere. Por exemplo, quando tratamos um caractere acentuado como 'ã', ele é contado como um único caractere, apesar de ser composto por dois bytes em sua representação UTF-8 interna. Se você quiser contar o número de bytes, use a função `byte_size/1`:

``` Elixir
IO.puts byte_size("ã") 
# Saída: 2
IO.puts String.length("ã")
# Saída: 1
```

## Veja Também
Se você quiser se aprofundar mais em string em Elixir, explore os links abaixo:

1. [Documentação oficial da Elixir sobre strings](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
3. [Trabalhando com strings em Elixir - Elixir School](https://elixirschool.com/pt/lessons/basics/strings/)