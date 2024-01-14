---
title:                "Elixir: Utilizando expressões regulares."
simple_title:         "Utilizando expressões regulares."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar Expressões Regulares em Elixir?

Um programador de Elixir provavelmente se depara com muitas manipulações de strings no seu dia a dia. Embora existam muitas funções nativas para trabalhar com strings, elas podem se tornar muito complexas e difíceis de manter. Nesse caso, as expressões regulares podem ser uma ferramenta útil para realizar buscas e manipulações de maneira mais eficiente e organizada.

## Como utilizar Expressões Regulares em Elixir

Em Elixir, as expressões regulares são criadas com o operador `=~`, que verifica se determinado padrão está presente em uma string e retorna um valor booleano. Vamos ver um exemplo usando o módulo `Regex`:

```
Elixir.regex(~r/pinguim|gato|cachorro/, "Eu amo pinguins!") # true
Elixir.regex(~r/pinguim|gato|cachorro/, "Eu amo pandas!") # false
```

Podemos também utilizar expressões regulares em conjunto com funções de string, como `String.match?` e `String.replace`, para buscar e substituir padrões em uma string:

```
Elixir.String.match?("ab", ~r/[a-z]b/) # true
Elixir.String.replace("gato preto", ~r/gato/, "gato branco") # "gato branco preto"
```

Além disso, Elixir também oferece opções de modificadores de expressões regulares, como `/i` para ignorar a diferença entre letras maiúsculas e minúsculas e `/u` para ignorar caracteres unicode.

## Aprofundando nas Expressões Regulares

As expressões regulares em Elixir seguem a mesma sintaxe utilizada em outras linguagens, mas com algumas particularidades. Por exemplo, para utilizar caracteres especiais, como ponto, asterisco e asterisco acentuado, é necessário utilizar uma barra invertida antes, como em `\.`, `\*` e `\*` respectivamente.

Outra coisa importante a ser levada em consideração é que as expressões regulares em Elixir são sempre case sensitive, ou seja, diferenciam entre letras maiúsculas e minúsculas, a menos que o modificador `/i` seja utilizado.

## Ver também
- [Documentação de Expressões Regulares em Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Exemplos práticos de Expressões Regulares em Elixir](https://blog.appsignal.com/2018/08/28/elixir-alright-intro-to-regex.html)
- [Forum de discussão sobre Expressões Regulares no Elixir Forum](https://elixirforum.com/t/regular-expressions-regex-in-elixir/359)