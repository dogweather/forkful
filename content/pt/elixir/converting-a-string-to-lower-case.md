---
title:                "Convertendo uma string para letras minúsculas"
html_title:           "Elixir: Convertendo uma string para letras minúsculas"
simple_title:         "Convertendo uma string para letras minúsculas"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que & Por quê? 
Converter uma string para letras minúsculas é o processo de transformar todas as letras maiúsculas em uma frase em letras minúsculas. Programadores geralmente fazem isso para padronizar o texto, facilitar comparações e evitar erros de digitação.

## Como fazer:
Converter uma string para letras minúsculas é muito simples em Elixir. Basta usar a função `String.downcase/1` que recebe a string como argumento e retorna a string em letras minúsculas. Veja um exemplo abaixo:

```
Elixir iex> String.downcase("ELIXIR")
"elixir"
```

Você também pode utilizar a sintaxe de pipe operator para encadear funções e converter a string de forma mais legível, como mostrado abaixo:

```
Elixir iex> "ELIXIR" |> String.downcase()
"elixir"
```

## Mais Informações:
Historicamente, o conceito de maiusculização e minusculização de letras em strings era mais relevante para linguagens que não possuíam o conceito de tipos de dados, como o Assembly. Em Elixir, esse processo pode ser feito de forma mais simples e eficiente usando funções da biblioteca padrão, como a `String.downcase/1`.

Se você estiver trabalhando com texto em português, é importante lembrar que a função `String.downcase/1` pode não funcionar corretamente para caracteres acentuados. Nesse caso, uma alternativa é utilizar a função `String.to_lower/1` do módulo `Unicode`, que lida com caracteres acentuados corretamente.

## Veja também:
- [Documentação oficial da função String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Documentação oficial da função String.to_lower/1](https://hexdocs.pm/elixir/Unicode.html#to_lower/1)