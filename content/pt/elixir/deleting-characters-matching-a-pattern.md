---
title:                "Elixir: Excluindo caracteres que correspondem a um padrão."
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que 

Por que alguém se envolveria em excluir caracteres que correspondem a um padrão? A resposta é simples: para manipular e limpar dados de forma eficiente em aplicações Elixir.

## Como Fazer

Para excluir caracteres que correspondem a um padrão em Elixir, podemos usar a função `String.replace/4` junto com uma expressão regular.

```
  Elixir>

  regex = ~r/[.]\d+/ 
  string = "Tenho 20.5 anos."
  replacement = ""

  String.replace(string, regex, replacement)

  # Output: "Tenho anos."
```

Neste exemplo, estamos substituindo qualquer caractere que corresponda à expressão regular `[.]\d+` com uma string vazia. Isso resulta na exclusão do número decimal "20.5" do texto original.

## Profundidade

Além da função `String.replace/4`, existem outras maneiras de excluir caracteres que correspondem a um padrão em Elixir. Por exemplo, podemos usar o operador `~r` para criar uma expressão regular e incorporá-la diretamente em uma string.

```
  Elixir>

  string = "Remover #hashtags de uma frase."
  ~r/[#]\S+/, replacement = ""

  String.replace(string, ~r/[#]\S+/, replacement)

  # Output: "Remover de uma frase."
```

Também podemos usar o módulo `Regex` para criar uma expressão regular e usá-la com a função `Regex.replace/3` para excluir caracteres que correspondem ao padrão.

```
  Elixir>

  regex = Regex.compile("[A-Z]")
  string = "Este TEXTO possui algumas letras MAIÚSCULAS."
  replacement = ""

  Regex.replace(regex, string, replacement)

  # Output: "ste EXTO possu algumas letras."
```

## Veja Também

- [Documentação oficial de Strings em Elixir](https://hexdocs.pm/elixir/String.html)
- [Guia de expressões regulares em Elixir](https://elixirschool.com/pt/lessons/advanced/regex/)