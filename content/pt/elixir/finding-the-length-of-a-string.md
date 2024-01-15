---
title:                "Encontrando o comprimento de uma string."
html_title:           "Elixir: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Você já parou para pensar em como o Elixir consegue determinar o tamanho de uma string? Apesar de parecer algo simples, esse é um assunto interessante e importante para qualquer programador.

## Como Fazer

Para encontrar o tamanho de uma string no Elixir, utilizamos a função `String.length/1`. Veja um exemplo abaixo:

```Elixir
iex> String.length("Olá, mundo!")
11
```

Podemos também utilizar essa função com variáveis:

```Elixir
iex> hello = "Olá, mundo!"
iex> String.length(hello)
11
```

Nesse caso, a variável `hello` está armazenando a string e a função `String.length/1` é chamada utilizando essa variável como argumento.

## Mergulho Profundo

Ao utilizar a função `String.length/1`, o Elixir conta o número de caracteres da string e retorna esse valor. Mas como ele faz isso?

Na verdade, o Elixir utiliza um conceito de listas encadeadas para armazenar strings. Ou seja, a string "Olá, mundo!" é na verdade uma lista com 11 elementos, onde cada elemento corresponde a um caractere.

Ao chamar a função `String.length/1`, o Elixir percorre essa lista e conta o número de elementos, determinando assim o tamanho da string.

Isso mostra como o Elixir utiliza uma abordagem simples e eficiente para encontrar o tamanho de uma string.

## Veja Também

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Artigo sobre estruturas de dados em Elixir](https://medium.com/@llucasreis/estruturas-de-dados-2ba010fcb2c0)
- [Vídeo explicando a função `String.length/1` em detalhes](https://www.youtube.com/watch?v=T1lvgI0_c3g)