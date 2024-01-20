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

# Capitalizando Strings em Elixir

## O Que e Por quê?

Capitalizar uma string é o ato de transformar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para melhorar a legibilidade e a apresentação dos dados.

## Como Faz:

Em Elixir, capitalizamos strings usando a função `String.capitalize/2`. Veja como:

```Elixir
string = "elixir é impressionante"
IO.puts String.capitalize(string, :pt)
```

O output será:

```Elixir
"Elixir é Impressionante"
```

## Aprofundando

A função `String.capitalize/2` do Elixir provém de suas raízes erlang. Historicamente, manipulações de string eram um pouco complexas, mas o Elixir conseguiu simplificar muito isso.

Com relação às alternativas, podemos fazer isso manualmente, dividindo a string em palavras, capitalizando cada palavra e juntando novamente as palavras. No entanto, a função `String.capitalize/2` torna o processo mais fácil e eficaz.

Quando falamos da implementação, a `String.capitalize/2` canaliza a string através de uma série de transformações, manipulando cada caracter individualmente. Primeiro, ela divide a string no espaço de caracteres, transforma a primeira letra de cada bloco em maiúscula e mantém o restante como está.

## Veja Também:

Para mais detalhes sobre a função `String.capitalize/2` e manipulação de strings em Elixir, você pode conferir a documentação oficial e alguns outros recursos relevantes.

- Documentação oficial da função String.capitalize: https://hexdocs.pm/elixir/String.html#capitalize/2
- Guia prático de manipulação de strings em Elixir: https://learningelixir.joekain.com/string-manipulations-in-elixir
- Curso de Elixir para iniciantes: https://www.udemy.com/course/elixir-for-beginners