---
title:                "Elixir: Unindo cadeias de caracteres"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# Por que concatenar strings em Elixir?

A concatenação de strings é uma tarefa comum em qualquer linguagem de programação, e em Elixir não é diferente. Ao unir duas ou mais strings, podemos criar novas strings que podem ser usadas em diferentes partes do nosso código. Isso é útil ao criar mensagens de saída para o usuário, por exemplo. 

## Como fazer?

Para concatenar strings em Elixir, podemos utilizar o operador `<>` ou a função `String.concat/2`. Vamos ver alguns exemplos utilizando o operador:

```Elixir
iex> "Hello " <> "world!"
"Hello world!"
iex> "Programming" <> " " <> "is fun!"
"Programming is fun!"
```

Podemos também usar a função `String.concat/2` da seguinte forma:

```Elixir
iex> String.concat(["I ", "love ", "Elixir!"])
"I love Elixir!"
```

Observe que, neste caso, passamos uma lista contendo as strings que queremos concatenar. 

## Deep Dive

Internamente, Elixir converte todas as strings em binários antes de concatená-las. Isso significa que, se estivermos concatenando uma grande quantidade de strings, podemos ter problemas de desempenho devido à alocação de memória. Para evitar isso, podemos utilizar a função `StringBuilder.concat/1` do módulo `String` ou o operador `<<>>`.

```Elixir
iex> String.concat(["I ", "love ", "Elixir!"])
"I love Elixir!"
iex> "I " <> "love " <> "Elixir!"
"I love Elixir!"
iex> StringBuilder.concat(["I ", "love ", "Elixir!"])
"I love Elixir!"
iex> "I " << "love " << "Elixir!"
"I love Elixir!"
```

Além disso, se precisarmos concatenar muitas strings com a mesma estrutura, podemos utilizar List comprehensions ou a função `Enum.reduce/3`. Vamos ver um exemplo utilizando a função `Enum.reduce/3`:

```Elixir
iex> Enum.reduce(1..10, fn (n, acc) -> acc <> "#{n} " end)
"1 2 3 4 5 6 7 8 9 10 "
```

## Veja também

- [Documentação oficial sobre strings em Elixir](https://hexdocs.pm/elixir/String.html)
- [Artigo sobre concatenação de strings em Elixir](https://medium.com/@glaucia86/concatenação-de-strings-em-elixir-31f7928c5a90)
- [Vídeo explicando diferentes formas de concatenar strings em Elixir](https://www.youtube.com/watch?v=I9oKrCLugYc)