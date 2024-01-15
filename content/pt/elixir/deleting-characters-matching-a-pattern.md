---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Elixir: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, ao trabalhar com strings em um programa Elixir, pode ser necessário remover caracteres que correspondam a um determinado padrão. Isso pode ser útil para limpar entradas de dados ou para transformar uma string em um formato específico.

## Como Fazer

Remover caracteres que correspondam a um padrão em Elixir é bastante simples. Você pode usar a função `String.replace/3`, que recebe três argumentos: a string original, o padrão a ser removido e a string de substituição. Veja um exemplo abaixo:

```Elixir
string = "123-45-6789"
pattern = ~r/-/  # O padrão é um traço
string_modificada = String.replace(string, pattern, "") # A string modificada não possui mais o traço
```

O resultado do código acima seria `"123456789"`.

Outro método útil é a função `String.replace_leading/3`, que remove apenas os caracteres no início da string que correspondam ao padrão especificado. Veja um exemplo:

```Elixir
string = "Hello World"
pattern = ~r/H/   # O padrão é a letra H
string_modificada = String.replace_leading(string, pattern, "")  # A string modificada tem o H removido do início
```

O resultado seria `"ello World"`.

## Profundando

Além das funções mencionadas acima, Elixir também oferece uma variedade de outras funções para manipulação de strings, como `String.strip`, `String.slice`, `String.split`, entre outras. Existem também diversas opções de padrões que podem ser utilizados, como expressões regulares e listas de caracteres. É importante verificar a documentação do Elixir para escolher a função e o padrão mais adequados para cada situação.

## Veja Também

- Documentação oficial do Elixir sobre strings: https://hexdocs.pm/elixir/String.html
- Tutorial sobre strings em Elixir: https://elixirschool.com/pt/lessons/basics/string/
- Exemplos de expressões regulares em Elixir: https://elixir-examples.github.io/string/regular-expressions/