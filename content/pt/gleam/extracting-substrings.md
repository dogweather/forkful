---
title:                "Extraindo substrings"
date:                  2024-01-20T17:45:39.999491-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extraindo substrings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que é e Por quê?

Extrair substrings é pegar partes específicas de uma string toda, como uma frase ou uma palavra. Programadores fazem isso para manipular, analisar ou validar textos, separando o que é importante do que não é.

## Como fazer:

A seguir estão exemplos de como você pode extrair substrings em Gleam:

```gleam
pub fn main() {
  let texto = "Olá, mundo maravilhoso!"
  let saudacao = string.slice(texto, 0, 5) // Pega "Olá, "
  let mundo = string.slice(texto, 6, 11) // Pega "mundo"
  let exclamacao = string.at(texto, 22) // Pega "!"

  saudacao // "Olá, "
  mundo // "mundo"
  exclamacao // Some("!")
}
```

E a saída seria:

```
"Olá, "
"mundo"
Some("!")
```

## Imersão

Extrair substrings é uma prática tão antiga quanto as primeiras linguagens de programação. Em Gleam, que é fortemente tipada e funcional, fazer isso é direto e geralmente livre de erros em tempo de execução, diferentemente de linguagens mais antigas como C onde um erro poderia corromper a memória.

Comparando com outras linguagens modernas, Gleam oferece segurança de tipo e imutabilidade como padrões que ajudam a prevenir muitos erros comuns ao lidar com strings. Por exemplo, ao invés de retornar `null` ou uma exceção para um índice inválido, `string.at` retorna um `Option`, forçando o programador a lidar com a possibilidade de uma falha de forma explícita.

Na implementação, as funções `slice` e `at` de Gleam são otimizadas para lidar com UTF-8, o que as torna robustas para manipulação de uma grande variedade de caracteres.

## Veja Também

- Documentação oficial do Gleam sobre strings: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Comunidade do Gleam no Reddit: [https://www.reddit.com/r/gleamlang/](https://www.reddit.com/r/gleamlang/)
