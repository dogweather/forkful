---
title:    "Elixir: Exclusão de caracteres correspondentes a um padrão"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

A exclusão de caracteres que correspondem a um padrão é uma tarefa comum na programação. Isso pode ajudar a limpar dados inconsistentes ou preparar uma string para ser manipulada de maneira eficiente.

## Como Fazer

Para excluir caracteres correspondentes a um padrão em Elixir, primeiro é preciso usar a função `String.replace/4`. Esta função aceita quatro argumentos - a string original, o padrão a ser correspondido, o texto a ser substituído e uma opção para especificar o número de substituições máximas a serem feitas.

Aqui está um exemplo de código em Elixir que excluirá todos os pontos de uma string:

```Elixir
original_string = "Meu endereço é a R. Almeida, 123."
pattern = "."
replacement = ""
String.replace(original_string, pattern, replacement)
```

A saída será: "Meu endereço é a R Almeida, 123".

## Deep Dive

É importante notar que a função `String.replace/4` retorna uma nova string em vez de modificar a string original. Também é possível usar expressões regulares como padrão para correspondência. Além disso, a opção para especificar o número máximo de substituições pode ser útil ao lidar com grandes quantidades de dados.

É sempre uma boa prática ler a documentação oficial do Elixir para obter mais informações sobre a função `String.replace/4` e suas opções de substituição disponíveis.

## Veja Também

- [Documentação oficial do Elixir para a função `String.replace/4`](https://hexdocs.pm/elixir/String.html#replace/4)
- [Tutorial do Elixir sobre expressões regulares](https://elixirschool.com/pt/lessons/advanced/regex/)
- [Artigo da Elixir School sobre manipulação de strings](https://elixirschool.com/pt/lessons/basics/strings/)