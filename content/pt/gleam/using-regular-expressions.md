---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Expressões regulares são padrões usados para encontrar correspondências de texto em strings. Programadores usam-nas por serem ferramentas poderosas para validação, busca e manipulação de texto de forma eficiente.

## Como fazer:
```gleam
import gleam/regex

fn main() {
  let pattern = regex.compile("^[a-zA-Z]+$").unwrap()
  let text = "GleamRocks123"

  case regex.run(pattern, text) {
    Ok(captures) -> io.println(captures)
    Error(_) -> io.println("No match")
  }
}
```
Saída de amostra:
```
No match
```

```gleam
fn main() {
  let pattern = regex.compile("(\\d+)").unwrap()
  let text = "Version2023"

  case regex.run(pattern, text) {
    Ok(captures) -> io.println(captures.at(1)) // Imprime o número capturado
    Error(_) -> io.println("No match")
  }
}
```
Saída de amostra:
```
Some("2023")
```

## Mergulho Profundo
Expressões regulares têm suas raízes na teoria da computação e foram popularizadas em ferramentas de edição de texto nos anos 70. Alternativas para expressões regulares incluem o uso de parsers dedicados ou bibliotecas de manipulação de strings. Na implementação, Gleam usa o pacote `gleam/regex` que fornece uma API simples e tipada com segurança para expressões regulares, mantendo o desempenho por trás dos panos.

## Ver Também
- Documentação oficial do Gleam sobre expressões regulares: [Gleam Regex](https://hexdocs.pm/gleam_stdlib/gleam/regex/)
- Um guia interativo para aprender expressões regulares: [RegexOne](https://regexone.com/)
- Testador de expressões regulares online, útil para experimentar e entender expressões: [Regex101](https://regex101.com/)