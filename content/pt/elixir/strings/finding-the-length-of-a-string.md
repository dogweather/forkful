---
title:                "Descobrindo o comprimento de uma string"
aliases:
- /pt/elixir/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:15.037114-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O quê & Por quê?
Encontrar o comprimento de uma string é básicamente medir quantos caracteres ela possui. Programadores fazem isso para validar entradas, definir limites de dados e manipular textos de forma precisa.

## Como Fazer:
```elixir
# 1. Usando a função String.length/1
comprimento = String.length("Olá, mundo!")
IO.puts(comprimento)  # Saída: 10

# 2. Contagem de caracteres Unicode válidos, não apenas bytes
comprimento_unicode = String.length("✈️🌍")
IO.puts(comprimento_unicode)  # Saída: 2
```

## Aprofundamento
Historicamente, a contagem de caracteres em uma string é uma operação comum em programação, mas ganhou complexidades com a adoção do padrão Unicode. No Elixir, a String é um módulo potente que lida com UTF-8, garantindo que a contagem de caracteres seja precisa, respeitando pontos de código Unicode e não simplesmente contando bytes. Alternativas como `byte_size/1` podem ser utilizadas quando a contagem de bytes é necessária ao invés de caracteres.

## Veja Também:
- [Documentação oficial do módulo String](https://hexdocs.pm/elixir/String.html)
- [Elixir School - Strings](https://elixirschool.com/pt/lessons/basics/strings/)
- [Entendendo Unicode e UTF-8](https://unicode.org/standard/WhatIsUnicode.html)
