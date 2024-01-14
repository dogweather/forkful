---
title:                "Gleam: Utilizando expressões regulares"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Gleam?

Expressões regulares são uma ferramenta poderosa e versátil para manipulação de strings em qualquer linguagem de programação. Em Gleam, elas são especialmente úteis para realizar buscas e substituições de padrões em textos. Se você precisa trabalhar com strings em seus projetos, aprender a usar expressões regulares pode facilitar muito o seu trabalho.

## Como usar expressões regulares em Gleam?

Em Gleam, expressões regulares são criadas através do módulo ```gleam/re``` e podem ser aplicadas a strings utilizando a função ```match```. Veja um exemplo de como usar expressões regulares para encontrar e substituir um padrão em uma string:

```Gleam
import gleam/re

let texto = "Olá, meu nome é João!"

let novo_texto = texto
  |> re.match("João", "Maria")
  |> result.get_or_else("Nome não encontrado")

assert novo_texto == "Olá, meu nome é Maria!"
```

Neste exemplo, utilizamos a função ```match``` para procurar pelo padrão "João" na string e substituí-lo por "Maria". O resultado é atribuído a uma variável e, caso o padrão não seja encontrado, uma string de fallback é retornada.

## Mais informações sobre expressões regulares em Gleam

As expressões regulares em Gleam seguem a mesma sintaxe do padrão definido pela linguagem de programação Erlang. Isso significa que é possível utilizar todas as funcionalidades e recursos suportados por essa linguagem.

Além disso, o módulo ```gleam/re``` oferece uma variedade de funções para manipulação de expressões regulares, como ```search```, ```replace_all```, ```split```, entre outras. Consulte a documentação oficial para obter mais informações sobre essas funções.

## Veja também
- Documentação oficial de expressões regulares em Gleam (https://gleam.run/articles/regular-expressions/)
- Tutoriais sobre expressões regulares em Gleam (https://gleam.run/book/guides/regular-expressions.html)
- Repositório com exemplos de uso de expressões regulares em Gleam (https://github.com/gleam-lang/gleam/tree/master/examples/regex)