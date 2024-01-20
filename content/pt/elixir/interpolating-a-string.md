---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolação de Strings em Elixir: Uma Visão Geral

## O Que & Por Quê?

A interpolação de strings é a técnica de inserir uma variável ou uma expressão num string de texto. Fazemos isso para tornar nossos programas mais dinâmicos e os códigos mais limpos.

## Como fazer:

Interpolar um string em Elixir é tão fácil quanto usar `#{}`. Vamos mostrar alguns exemplos:

```Elixir
nome = "Miguel"
IO.puts "Olá, #{nome}!"
# Produzirá: Olá, Miguel!
```

Você também pode usar expressões dentro das chaves:

```Elixir
count = 5
IO.puts "Você tem #{count * 2} maçãs."
# Produzirá: Você tem 10 maçãs.
```

## Mais Fundo

A interpolação de strings foi ao longo do tempo um ponto de convergência para várias linguagens de programação modernas, como Elixir, Ruby e JavaScript. Antes de sua existência, os desenvolvedores geralmente usavam concatenação que pode tornar o código difícil de ler e manter.

Como alternativa à interpolação, você sempre pode usar concatenação de strings. No entanto, a interpolação é geralmente mais limpa e mais fácil de ler, pois você pode evitar a quebra excessiva de linhas e adicionar espaços de forma inconsistente.

Internamente, quando você interpola strings no Elixir, está apenas chamando a função `Kernel.to_string/1` nos termos dentro das chaves `{}`. Então ele junta todos os fragmentos de string juntos, que se torna o novo string.

## Veja Também

1. Documentação Elixir sobre [Strings e Binários](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#interpolation)
2. Discussão no fórum Elixir [sobre interpolação de Strings](https://elixirforum.com/t/what-does-interpolation-in-elixir-mean/21402)