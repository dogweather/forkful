---
title:                "Interpolando uma string"
html_title:           "Elixir: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Interpolar uma string significa combinar variáveis ou expressões em uma string para formar uma nova string. Os programadores usam essa técnica para evitar a concatenação manual de strings e tornar seu código mais eficiente e legível.

## Como fazer:
```Elixir
# Exemplo 1: String simples com interpolação de variáveis
nome = "Maria"
idade = 25
IO.puts "Olá, meu nome é #{nome} e tenho #{idade} anos."
# Output: Olá, meu nome é Maria e tenho 25 anos.

# Exemplo 2: Interpolação de expressões
x = 5
IO.puts "O quadrado de #{x} é #{x * x}."
# Output: O quadrado de 5 é 25.
```

## Mergulho Profundo:
Interpolação de string é uma técnica popular em várias linguagens de programação, incluindo Elixir. Ela torna o código mais limpo e fácil de ler, facilita a adição de variáveis e ajuda a evitar erros de concatenação de strings. Além disso, ao utilizar a interpolação, o próprio compilador do Elixir é responsável por avaliar as expressões, o que pode melhorar o desempenho do código.

## Veja também:
- [Elixir - Documentação oficial sobre strings](https://hexdocs.pm/elixir/String.html)
- [Interpolação de string em Elixir - Tutorial](https://elixircasts.io/string-interpolation-in-elixir)
- [Elixir - Guia de estilo de código](https://github.com/christopheradams/elixir_style_guide#interpolation)