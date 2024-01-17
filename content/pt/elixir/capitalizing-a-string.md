---
title:                "Capitalizando uma string"
html_title:           "Elixir: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O que & Por que?

Capitalizar uma string é o ato de transformar a primeira letra de uma palavra em maiúscula. Programadores geralmente fazem isso para padronizar a formatação de textos e facilitar a leitura do código.

## Como fazer:

```Elixir
# Exemplo 1: Capitalizando uma única string
String.capitalize("elixir")  # Saída: "Elixir"

# Exemplo 2: Capitalizando todas as letras de uma string
String.upcase("elixir")  # Saída: "ELIXIR"

# Exemplo 3: Capitalizando apenas a primeira letra de cada palavra em uma string
String.capitalize_words("elixir is awesome")  # Saída: "Elixir Is Awesome"
```

## Profundidade:

A capitalização de strings é uma prática comum em muitas linguagens de programação. Ela se originou do uso de estilos de escrita como o CamelCase e o PascalCase, que são amplamente utilizados em nomes de variáveis e funções. Existem também outras formas de capitalizar strings, como o estilo snake_case e o kebab-case.

A implementação da função de capitalização em Elixir é baseada no Unicode, o que significa que ela suporta caracteres especiais e acentos em diferentes idiomas. Além disso, Elixir também fornece as funções `uncapitalize` e `downcase`, que fazem o oposto da `capitalize` e `upcase`, respectivamente.

## Veja também:

- Documentação oficial sobre a função `String.capitalize`: https://hexdocs.pm/elixir/String.html#capitalize/1
- Artigo sobre estilos de escrita de código: https://github.com/elixir-lang/elixir/blob/master/lib/elixir/pages/Code%20Style.md