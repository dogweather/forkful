---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:47:32.634688-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Descobrir o comprimento de uma string significa determinar o número de caracteres que ela contém. Programadores fazem isso para validações, manipulações de texto e para garantir adequação aos limites de sistema ou de regras de negócios.

## Como fazer:
Em Gleam, você pode usar a função `String.length` para obter o comprimento de uma string. Aqui estão alguns exemplos simples:

```gleam
import gleam/io

pub fn main() {
  let mensagem = "Olá, mundo!"
  let tamanho = String.length(mensagem)
  io.println(tamanho) // Imprime: 12
}

pub fn exemplo_com_string_vazia() {
  let vazia = ""
  io.println(String.length(vazia)) // Imprime: 0
}
```

## Mergulho Profundo
Antigamente, medir o comprimento de uma string era uma operação simples. Cada caractere tinha o mesmo tamanho. Com a internacionalização e codificação de caracteres como UTF-8, a operação ficou mais complexa. No UTF-8, um único caractere pode ter de 1 a 4 bytes. A função `String.length` em Gleam conta os 'code points' de Unicode, que podem não corresponder diretamente ao número de 'grapheme clusters' (o que às vezes consideramos um caracter visual).

Alternativas para determinar diferentes aspectos do tamanho de uma string incluem contar bytes ou contar 'grapheme clusters'. Em certas linguagens, isso pode ser uma operação mais complicada e propensa a erros, mas Gleam abstrai essas complexidades através de sua API de strings.

## Veja Também
- Documentação oficial da Gleam para a função `String.length`: https://hexdocs.pm/gleam_stdlib/gleam/string/
- Unicode Text Segmentation para entender melhor 'grapheme clusters' vs 'code points': http://unicode.org/reports/tr29/
- Artigo da Mozilla sobre codificações de caracteres e suas complexidades: https://developer.mozilla.org/en-US/docs/Web/Guide/Localizations_and_character_encodings
