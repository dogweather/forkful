---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Capitalizar uma string significa transformar a primeira letra de uma palavra ou frase em maiúscula. Programadores fazem isso para formatar textos de forma consistente ou atender a regras gramaticais, especialmente em interfaces de usuário.

## How to:
Capitalizar uma string em Gleam é tranquilo. Vou te mostrar como, olha só:

```gleam
import gleam/string

pub fn main() {
  let greeting = "olá, mundo!"
  let capitalized_greeting = string.capitalize(greeting)
  capitalized_greeting
}
```

Isso vai sair:

```
"Olá, mundo!"
```

Simples, né?

## Mergulho Profundo
No passado, cada linguagem de programação introduzia sua própria maneira de manipular strings. Em Gleam, uma linguagem funcional e estática tipada que compila para Erlang VM, usamos funções de alto nível da biblioteca padrão como `string.capitalize` para esse trabalho. 

Uma alternativa manual seria passar pela string, pegar o primeiro caractere, transformá-lo em maiúscula com `string.uppercase` e depois concatenar com o resto da string. Mas isso é reinventar a roda, sem falar em um possível desperdício de desempenho.

A implementação por trás da função `capitalize` lida com os detalhes, como caracteres UTF-8, que podem ter tamanhos variáveis – algo não trivial.

## Veja Também
Se quer se aprofundar mais, dá uma olhada nesses recursos:

- Para entender mais sobre Unicode e UTF-8: [https://unicode.org/](https://unicode.org/)
- Uma discussão sobre a performance de manipulação de strings em Erlang (que é relevante para Gleam): [https://ferd.ca/rtb-where-erlang-blooms.html](https://ferd.ca/rtb-where-erlang-blooms.html)
