---
title:                "Elixir: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

A conversão de strings para minúsculas é essencial para manipular e comparar strings em Elixir. Ao converter uma string para minúsculas, você pode garantir que a comparação entre duas strings será precisa, independentemente do uso de letras maiúsculas e minúsculas. Isso é especialmente importante quando se trabalha com entradas de usuários, que podem não seguir um padrão específico de capitalização.

## Como fazer isso:

Para converter uma string para minúsculas em Elixir, você pode usar a função `String.downcase/1`. Veja um exemplo abaixo:

```
Elixir
iex> String.downcase("ELIXIR")
"elixir"

iex> String.downcase("Elixir 123")
"elixir 123"
``` 

Como você pode ver, todas as letras maiúsculas foram convertidas para minúsculas. Além disso, caracteres especiais e números não são afetados pela conversão.

## Profundidade:

Ao converter uma string para minúsculas, é importante entender como isso acontece por trás dos bastidores. Em Elixir, as strings são representadas como listas de caracteres. Ao usar a função `String.downcase/1`, o Elixir itera sobre cada caractere na lista e, quando encontra uma letra maiúscula, usa a função `String.downcase/1` para convertê-la para minúscula. Isso é feito usando o código Unicode de cada caractere, garantindo que a conversão seja precisa e funcione para todos os idiomas.

Para obter mais informações sobre strings e como elas são representadas em Elixir, você pode consultar a documentação oficial ou o livro "Programming Elixir", de Dave Thomas.

## Veja também:

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Livro "Programming Elixir" de Dave Thomas](https://pragprog.com/book/elixir/programming-elixir)