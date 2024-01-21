---
title:                "Concatenando strings"
date:                  2024-01-20T17:34:28.761353-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenando strings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Concatenar strings é basicamente juntar dois ou mais pedaços de texto para formar uma única sequência. Programadores fazem isso toda hora, seja para montar mensagens, criar comandos dinâmicos, ou simplesmente organizar dados de modo mais legível.

## Como Fazer:
Elixir torna a concatenação de strings bem fácil. Olha só:

```elixir
# Concatenação simples com operador <>
string_a = "Olá, "
string_b = "mundo!"
resultado = string_a <> string_b
IO.puts resultado
# Saída: Olá, mundo!
```

Usar o operador `<>` é prático para juntar duas strings. Mas e se você tiver uma lista de strings para unir? Utilize a função `Enum.join/2`:

```elixir
# Juntando uma lista de strings com Enum.join
lista_de_strings = ["Elixir", "é", "incrível!"]
resultado = Enum.join(lista_de_strings, " ")
IO.puts resultado
# Saída: Elixir é incrível!
```

## Aprofundamento
Na história da programação, a concatenação de strings é tão antiga quanto as próprias strings. Em Elixir, a imutabilidade implica que, tecnicamente, ao concatenar strings, estamos criando uma nova string com o conteúdo das anteriores.

Outras línguas têm suas abordagens — algumas com operadores específicos (como o `+` em Python), outras com métodos embutidos (como o `.concat()` em Javascript), mas Elixir escolheu usar o `<>`.

Internamente, Elixir trata strings como binários (uma sequência de bytes). Isso quer dizer que ao concatenar, o Elixir precisa copiar os bytes da primeira string, seguido pelos bytes da segunda, para montar uma nova sequência binária. Isso é eficiente e rápido na máquina virtual Erlang (BEAM), mas é algo a se pensar ao lidar com strings muito grandes.

## Ver Também
- A documentação oficial da função `<>`: https://hexdocs.pm/elixir/String.html#<>
- A documentação da função `Enum.join/2`: https://hexdocs.pm/elixir/Enum.html#join/2
- Para entender melhor o sistema binário de Elixir: https://hexdocs.pm/elixir/Binary.html 
- Sobre imutabilidade em Elixir: https://elixir-lang.org/getting-started/basic-types.html#immutability