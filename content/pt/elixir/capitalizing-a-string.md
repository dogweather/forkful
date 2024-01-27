---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string significa transformar o primeiro caracter de uma palavra em maiúsculo. Programadores fazem isso por questões de formatação, para seguir normas gramaticais ou para garantir que a saída dos dados seja consistente e profissional.

## How to:
Em Elixir, capitalizar uma string é simples com a função `String.capitalize/1`. Aqui estão alguns exemplos:

```elixir
String.capitalize("elixir")
String.capitalize("programação")
String.capitalize("olá, mundo!")
```

Saída de exemplo para as chamadas acima:

```elixir
"Elixir"
"Programação"
"Olá, mundo!"
```

## Deep Dive
Historicamente, a necessidade de capitalizar strings vem do desejo de padronizar a apresentação de texto, especialmente em nomes próprios e no início das sentenças. Em Elixir, a função `String.capitalize/1` não se limita a simplesmente transformar o primeiro caracter em maiúsculo; ela também transforma quaisquer caracteres subsequentes em minúsculas, o que é útil para corrigir o case de palavras inseridas incorretamente.

Uma alternativa à função `String.capitalize/1` é usar funções de combinação de caracteres para manipular strings a um nível mais baixo, mas isso raramente é necessário e aumenta a complexidade do código sem necessidade.

Quanto à implementação, `String.capitalize/1` lida com Unicode de forma adequada, o que significa que pode capitalizar corretamente caracteres fora do padrão ASCII, como letras acentuadas e outros alfabetos.

## See Also
- Documentação oficial da função `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/1
- Aula sobre strings em Elixir com foco em Unicode e manipulação de strings: https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html
- Regex e padrões de Elixir para manipulação avançada de strings: https://hexdocs.pm/elixir/Regex.html
