---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Convertendo uma string para minúsculas em Gleam

## O que & Por quê?

Converter uma string para minúsculas significa transformar todas as letras maiúsculas presentes na string em minúsculas. Isso auxilia na uniformização dos dados, facilitando a comparação e a busca de strings.

## Como fazer:

Aqui está um exemplo de como você pode converter uma string para minúsculas em Gleam:

```gleam
import gleam/string

let minha_string = "Olá Mundo!"
let string_minuscula = string.to_lower(minha_string)

assert string_minuscula == "olá mundo!"
```
Basta importar o módulo `gleam/string` e usar a função `to_lower`. No exemplo acima, a variável `string_minuscula` receberá o valor "olá mundo!".

## Mergulho Profundo

A função `to_lower` do módulo `gleam/string` existe desde a primeira versão da linguagem Gleam, demonstrando seu papel fundamental no trabalho com strings. Existem alternativas, como implementar sua própria função para converter strings em minúsculas, mas estas geralmente não são tão eficientes ou diretas quanto o método integrado.

Tecnicamente, a função `to_lower` examina cada caractere na string e, se for uma letra maiúscula, substitui pelo seu equivalente em minúscula. Este processo é baseado na tabela ASCII, que atribui números distintos a letras maiúsculas e minúsculas.

## Veja também:

Para obter mais informações sobre o trabalho com strings em Gleam, confira a documentação oficial [aqui](https://gleam.run/book/tour/strings.html).

Para aprender mais sobre a tabela ASCII e como as letras maiúsculas e minúsculas são representadas, veja [este link](http://www.asciitable.com/).