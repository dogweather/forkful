---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Converter uma string para letras minúsculas é transformar todos os caracteres maiúsculos da string em seus equivalentes minúsculos. Os programadores fazem isso para normalizar as entradas de dados e evitar erros de comparação sensíveis a maiúsculas e minúsculas.

## Como Fazer:

Em Elixir, a solução mais direta é usar a função `String.downcase/1`. Aqui está um exemplo simples:

```elixir
s = "Eu Amo Programar com Elixir"
IO.puts String.downcase(s)
```

A saída é

```elixir
"eu amo programar com elixir"
```

Perfeito - essa string está agora toda em letras minúsculas.

## Deep Dive

Elixir é uma linguagem relativamente nova, tendo sido criada em 2011, mas os conceitos de conversão de strings para letras minúsculas datam do início da computação. A abordagem de Elixir para isso é altamente influenciada por sua linguagem pai, Erlang, e enfatiza a simplicidade e a clareza de propósito.

Existem alternativas para 'String.downcase/1' em Elixir, como 'String.to_charlist/1' seguido de 'Enum.map/2'. Mas a maioria das situações em Elixir simplesmente exigiria o uso de 'String.downcase/1', pois é o mais eficiente e direto.

A implementação de 'String.downcase/1' é baseada na tabela Unicode. Isto é, para todos os caracteres aplicáveis, essa função mapeia o caractere para a versão minúscula correspondente na tabela Unicode. Portanto, a função trabalha não apenas com o script latino, mas também com qualquer script que tenha noções de letras maiúsculas e minúsculas.

## Veja Também:

Elixir tem uma excelente documentação online que cobre a módulo de strings em detalhes. Aqui estão alguns links relevantes:
- [Funções auxiliares para trabalhar com strings unicode e binárias](https://hexdocs.pm/elixir/String.html)
- [Tutorial do Elixir para iniciantes](https://elixirschool.com/pt/)
- [Elixir no Exercism, um recurso prático para aprender com exercícios](https://exercism.io/tracks/elixir)