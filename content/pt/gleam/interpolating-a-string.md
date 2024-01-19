---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Interpolação de strings é uma conveniência que permite inserir variáveis diretamente em strings. Programadores usam isso para construir dinamicamente strings de uma maneira mais legível e eficiente.

## Como Fazer:

Na versão atual do Gleam, você pode usar os operadores de concatenação para combinar strings. Veja um exemplo:

```Gleam
let nome = "João"
let mensagem = "Olá, " ++ nome
mensagem // "Olá, João"
```

Para números e outros tipos, use a função `int_to_string` ou funções semelhantes antes da concatenação:

```Gleam
let idade = 25
let mensagem = "Eu tenho " ++ int_to_string(idade) ++ " anos"
mensagem // "Eu tenho 25 anos"
```

## Aprofundando o Assunto:

Historicamente, a interpolação de strings foi popularizada por linguagens como Perl e Ruby. No Gleam, a interpolação nativa de strings ainda não é apresentada, então usamos a concatenação de strings.

Há linguagens que têm funções especiais para interpolação de strings, como Java com `String.format()` ou Python com f-strings. Apesar de mais diretas, podem ser menos eficientes.

Os detalhes de implementação de concatenação no Gleam são bastante simples. Ele só combina as duas strings em uma única string.

## Veja Também:

Para mais informações sobre trabalhar com strings em Gleam, confira a documentação oficial: [Documentação Gleam](https://gleam.run/docs/tour/strings/)

Para uma introdução mais geral à interpolação de strings em outras linguagens, a página da Wikipedia é um bom ponto de partida: [Interpolação de String - Wikipedia](https://en.wikipedia.org/wiki/String_interpolation)