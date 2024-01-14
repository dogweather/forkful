---
title:                "Elixir: Extraindo Substrings"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Elixir

Extrair substrings é uma ferramenta útil para programadores Elixir que desejam realizar operações específicas em partes específicas de uma string. Ao invés de usar a string inteira, extrair substrings permite que você selecione e manipule uma porção do texto de forma mais eficiente.

## Como fazer

Para extrair substrings em Elixir, utilize a função `String.slice/3`. O primeiro argumento é a string original, o segundo é o ponto de início da substring e o terceiro é o tamanho da substring. Aqui está um exemplo de código que extrai uma substring de tamanho 4 começando no índice 2:

```Elixir
string = "Olá mundo!"
String.slice(string, 2, 4)
# saída: "á"

```

Você também pode fornecer um índice negativo como ponto de início, para contar a partir do final da string. Por exemplo:

```Elixir
string = "Olá mundo!"
String.slice(string, -3, 2)
# saída: "do"
```

Além disso, você pode usar o operador `..` (chamado de "operador de intervalo") para especificar um intervalo de índices ao invés de um ponto de início e tamanho. Por exemplo:

```Elixir
string = "Hello world!"
String.slice(string, 3..7)
# saída: "lo wo"
```

## Mergulho profundo

A função `String.slice/3` é bastante versátil e permite que você extraia substrings de forma flexível. Além disso, ela possui uma variedade de opções adicionais, como o uso de expressões regulares para encontrar padrões em uma string e extrair a correspondência desejada.

Além disso, você também pode utilizar a função `String.split/2` para extrair uma lista de substrings em vez de uma única substring. Basta fornecer o caractere ou string que deseja usar como ponto de divisão para a string original. Por exemplo:

```Elixir
string = "Olá mundo!"
String.split(string, " ")
# saída: ["Olá", "mundo!"]
```

## Veja também

- Documentação sobre a função `String.slice/3`: https://hexdocs.pm/elixir/String.html#slice/3
- Mais informações sobre expressões regulares em Elixir: https://elixir-lang.org/getting-started/pattern-matching.html#regular-expressions