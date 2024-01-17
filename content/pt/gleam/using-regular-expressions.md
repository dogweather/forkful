---
title:                "Utilizando expressões regulares"
html_title:           "Gleam: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

O que é Expressões Regulares e por que os programadores as utilizam?

As expressões regulares são sequências de caracteres utilizadas para procurar e manipular padrões de texto em strings. Elas são comumente usadas por programadores para buscar e substituir strings, validar formatos de input e realizar outras tarefas relacionadas ao processamento de texto. 

Como realizar operações com Expressões Regulares no Gleam:

```Gleam
import gleam/re
import gleam/io

// Verificar se uma string contém apenas números
let string = "123"

let is_num = string
  |> String.to_char_list
  |> List.all(x -> Char.is_numeric(x))

assert is_num == Ok(true)

// Substituir caracteres não alfanuméricos por "_"
let input = "Hello,!Gleam@123?"

let output = input
  |> Re.replace(#"[^a-zA-Z0-9]"#, "_")

assert output == Ok("Hello_Gleam_123_")

```

Aprofundando-se nas Expressões Regulares:

As expressões regulares foram criadas em 1951 pelo matemático Stephen Kleene como uma forma de descrever linguagens formais. Elas são amplamente usadas em diferentes linguagens de programação, como Perl, Python e, mais recentemente, Gleam. Outra forma de realizar operações semelhantes a expressões regulares é através de funções de manipulação de strings, no entanto, isso pode ser mais trabalhoso e menos eficiente em termos de desempenho. Ao utilizar expressões regulares, os programadores podem escrever menos código e alcançar resultados mais precisos e rápidos na manipulação de texto.

Links Relacionados:

- Documentação oficial do Gleam: https://gleam.run/
- Tutorial de expressões regulares em Gleam: https://gleam.run/book/tutorials/regular_expressions.html
- Artigo sobre as origens das expressões regulares: https://www.regular-expressions.info/history.html