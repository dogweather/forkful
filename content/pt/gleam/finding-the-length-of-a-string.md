---
title:                "Encontrando o comprimento de uma string"
html_title:           "C: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Descubrindo o comprimento de uma string em Gleam

## O que e por que?
Determinar o comprimento de uma string significa saber quantos caracteres existem em uma determinada sequência de texto. Os programadores fazem isso para manipular dados de string, seja para validação de entrada de usuário, manipulação de texto e muitas outras funções.

## Como fazer:
Aqui está um exemplo de como obter o comprimento de uma string em Gleam.

```gleam
import gleam/string

fn main() {
    let exemplo = "Olá, mundo!"
    let comprimento = string.len(exemplo)
    print(comprimento)
}
```
Saída:

```gleam
12
```

Neste exemplo, a função `len` do módulo `string` foi usada para encontrar o comprimento da string "Olá, mundo!". A saída foi `12`, indicando que a string contém 12 caracteres.

## Deep Dive

1. Contexto histórico
Em linguagens de programação mais antigas como o C, a determinação do comprimento de uma string era mais complicada e propensa a erros. You had to loop through the string until you found the NULL terminating character. Gleam facilita isso com a função `len`.

2. Alternativas
Se a performance for um problema e você estiver lidando com strings ASCII, poderia contar os bytes em vez dos caracteres. No entanto, isto não funcionará corretamente com caracteres Unicode que exigem mais de um byte para serem representados.

3. Detalhes de implementação
Gleam trata strings como UTF-8. Portanto, a função `len` conta o número de "code points" que podem não corresponder ao número total de bytes se a string incluir caracteres não ASCII.

## Veja também

Documentação do módulo de string Gleam:  https://hexdocs.pm/gleam_stdlib/gleam/string/

Guia do usuário Gleam: https://gleam.run/book/tour/strings.html

UTF-8 e Unicode: https://www.joelonsoftware.com/2003/10/08/the-absolute-minimum-every-software-developer-absolutely-positively-must-know-about-unicode-and-character-sets-no-excuses/