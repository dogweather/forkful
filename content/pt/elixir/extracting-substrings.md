---
title:                "Extraindo subcadeias de caracteres"
html_title:           "Elixir: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que

Extrair subtrings é extremamente útil na programação Elixir quando se trata de manipulação de strings. Ao dividir uma string em partes menores, podemos facilitar a filtragem, busca e substituição de informações específicas.

## Como Fazer

Extrair subtrings em Elixir é um processo bem simples. Você pode usar a função `slice/2` ou `substring/3` para realizar essa tarefa.

Aqui está um exemplo de como a função `slice/2` pode ser utilizada para extrair uma substring de uma string.

```
Elixir iex> str = "Programando em Elixir é divertido!"
iex> String.slice(str, 11..20)
" Elixir é "
```

Já a função `substring/3` permite que você defina a posição inicial e final da substring desejada.

```
Elixir iex> str = "Programando em Elixir é divertido!"
iex> String.substring(str, 22, 10)
"divertido"
```

Ambas as funções possuem uma vasta gama de opções para lidar com diferentes tipos de strings, como unicode, format string, entre outros.

## Aprofundando

Para entender melhor como essas funções funcionam, é importante saber que em Elixir as strings são representadas como uma lista de caracteres. Isso significa que podemos usar as mesmas funções que usamos em listas para manipular strings.

A função `slice/2`, por exemplo, usa o operador `..` para indicar o intervalo de posições que devem ser selecionadas na lista de caracteres. Já a função `substring/3` usa o operador `:` para indicar o índice inicial e o final.

Você também pode utilizar as funções `bin_to_list/1` e `list_to_bin/1` para converter entre string e lista de caracteres, tornando mais fácil a manipulação desses tipos de dados.

## Veja também

- [Documentação oficial do Elixir sobre strings](https://hexdocs.pm/elixir/String.html)
- [Mais exemplos de uso da função `slice/2` e `substring/3`](https://medium.com/@diamondgfx/elixir-string-slicing-examples-3d1b7ade981f)