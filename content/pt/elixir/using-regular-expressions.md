---
title:                "Elixir: Utilizando expressões regulares"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em Elixir

As expressões regulares são um poderoso recurso usado por programadores para realizar buscas e manipular strings de forma eficiente. Em Elixir, elas podem ser usadas em diversos contextos, desde validação de formulários até a análise de dados. Portanto, é importante adquirir conhecimento e prática com esse recurso para se tornar um programador mais eficiente e produtivo.

## Como utilizar expressões regulares em Elixir

Para utilizar expressões regulares em Elixir, basta utilizar a função `=~`seguida pelo padrão que se deseja procurar, dentro de `/ /` delimitadores. Além disso, é possível utilizar o operador `=~` em conjunto com a função `Regex.match?` para verificar se uma string corresponde ao padrão especificado.

```
Elixir
# Procurando por um padrão em uma string
"Hello, World!" =~ /Hello/ # true

# Utilizando o operador =~ e a função Regex.match?
Regex.match?(~r/[0-9]+/, "12345") # true
Regex.match?(~r/[a-z]+/, "12345") # false
```

Para utilizar grupos de captura em expressões regulares, basta adicionar parênteses em torno do padrão que se deseja capturar. Em seguida, é possível acessar o valor da captura utilizando o terceiro argumento da função `Regex.match?`.

```
Elixir
# Utilizando grupos de captura
text = "Meu número de telefone é (123) 456-7890"
Regex.match?(~r/(\d{3}) (\d{3}-\d{4})/, text, 1) # "123"
Regex.match?(~r/(\d{3}) (\d{3}-\d{4})/, text, 2) # "456-7890"
```

## Aprofundando no uso de expressões regulares em Elixir

As expressões regulares em Elixir são baseadas na regex do Perl e podem ser usadas para realizar substituições em strings utilizando a função `Regex.replace`. Também é possível utilizar a bandeira `i` para ignorar diferenças entre letras maiúsculas e minúsculas e a bandeira `s` para permitir a correspondência em várias linhas.

Além disso, os módulos Regex e String fornecem diversas funções úteis para trabalhar com expressões regulares, como `Regex.split`, `Regex.scan` e `String.replace`.

```
Elixir
# Substituindo uma string utilizando expressões regulares
text = "Hello, World!"
Regex.replace(~r/Hello/, text, "Hi") # "Hi, World!"

# Utilizando a bandeira s para correspondência em várias linhas
text = "Hello\nWorld!"
Regex.match?(~r/W.+/, text) # false
Regex.match?(~r/W.+/, text, s: true) # true

# Utilizando as funções do módulo Regex
Regex.split(~r/[ ,\-]/, "Hello, World!") # ["Hello", "", "World!"]
Regex.scan(~r/\d+/, "123, 456, 789") |> Enum.map(&String.to_integer/1) # [123, 456, 789]
```

## Veja também

- [Documentação das expressões regulares em Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial de expressões regulares em Elixir](https://elixirschool.com/lessons/specifics/regex/)
- [Regexr - ferramenta online para testar expressões regulares em Elixir](https://regexr.com/)