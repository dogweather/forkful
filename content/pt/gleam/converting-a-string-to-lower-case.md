---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:17.272472-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que é & Porquê?
Converter uma string para minúsculas significa transformar todos os caracteres alfabéticos da string para a sua forma minúscula correspondente. Programadores fazem isso para padronizar os dados de entrada, facilitar comparações consistentes e atender a certos requisitos do sistema ou domínio de aplicação.

## Como fazer:
Gleam torna a conversão de string para minúsculas direta e fácil. Confira o exemplo:

```gleam
import gleam/string

pub fn main() {
  let greeting = "Olá, Mundo!"
  let lowercased_greeting = string.lowercase(greeting)
  lowercased_greeting
}
```

Saída esperada:
```
"olá, mundo!"
```

## Mergulho Profundo
Historicamente, converter strings para minúsculas é um conceito que transcende linguagens de programação específicas, tendo raízes nas práticas de catalogação e indexação de dados, onde a consistência é chave. Em Gleam, além da função `string.lowercase`, podemos considerar a utilização de alternativas como o mapeamento manual de caracteres para suas formas minúsculas, mas isso não é recomendado pois é trabalhoso e propenso a erros. A implementação de conversão de string para minúsculas em Gleam considera as especificidades de codificação Unicode, garantindo que a transformação de caracteres seja feita de maneira correta e segura em diferentes idiomas e conjuntos de caracteres.

## Veja Também
Confira mais sobre operações de strings e tratamentos de caracteres em Gleam:

- [Unicode Standard](http://www.unicode.org/standard/standard.html)
- [Práticas de normalização de dados](https://en.wikipedia.org/wiki/Text_normalization)