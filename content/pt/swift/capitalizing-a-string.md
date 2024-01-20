---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que É & Por Que Fazer?
Capitalizar uma string é simplesmente transformar as letras iniciais de cada palavra em maiúsculas. Programadores fazem isso para formatar textos, seja para títulos, nomes próprios ou para padronizar a entrada de dados.

## Como Fazer:
```Swift
let pequenaFrase = "olá, mundo da programação!"
let fraseCapitalizada = pequenaFrase.capitalized

print(fraseCapitalizada)
// Saída: "Olá, Mundo Da Programação!"
```
Se você quer capitalizar só a primeira letra:
```Swift
let apenasPrimeiraMaiuscula = pequenaFrase.prefix(1).uppercased() + pequenaFrase.dropFirst()

print(apenasPrimeiraMaiuscula)
// Saída: "Olá, mundo da programação!"
```

## Mergulho Profundo
Historicamente, capitalizar strings é uma convenção vinda da escrita e gramática normativa. Em programação, essa prática começou para refletir padrões de texto humano. Existem diferentes maneiras de capitalizar strings. A função `.capitalized` do Swift é uma convenção que segue as regras de capitalização específicas: ela capitaliza a primeira letra de cada palavra.

Além do `.capitalized`, Swift tem funções como `.uppercased()` que transformam todos os caracteres em maiúsculas, e `.lowercased()` para todos em minúsculas. Importante saber que a capitalização é dependente do locale: idiomas como o turco têm regras de capitalização especiais que o Swift leva em conta.

Sob o capô, essas funções usam a Unicode Standard para determinar o que é uma "letra" e como transformar de minúscula para maiúscula e vice-versa. Cada caractere tem um código específico que define sua representação.

Quando o assunto é performance, capitalizar strings é relativamente trivial para textos curtos. Entretanto, para grandes volumes de texto, é importante considerar a eficiência do método utilizado.

## Veja Também
- Swift Standard Library: [String](https://developer.apple.com/documentation/swift/string)
- Unicode Standard: [Character Properties](https://www.unicode.org/reports/tr44/#CharacterProperties)