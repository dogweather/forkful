---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Interpolação de string é uma técnica que permite inserir uma variável ou expressão diretamente dentro de uma string. Os programadores fazem isso para facilitar a manipulação de strings e tornar o código mais legível.

## Como fazer:

Ruby facilita a interpolação de strings com uma sintaxe especial chamada de _string interpolation_. Para isto, você precisa usar aspas duplas ("") em vez de aspas simples (''). Aqui está um exemplo:

```Ruby
nome = "João"
puts "Olá, #{nome}!" # => Olá, João!
```

O código dentro dos sinais de #{ } é uma expressão que é avaliada e então convertida para uma string.

## Em Profundidade:

A interpolação de string no Ruby, como em muitas outras linguagens de programação como Perl e PHP, vem de uma tradição em linguagens de shell Unix. 

Uma alternativa à interpolação de string é a concatenação de string, que pode ser mais verbosa e menos clara.

Detalhes de implementação são surpreendentemente complicados, como a interpolação de string na verdade envolve a criação de uma nova string com os valores interpolados. Isso pode ter implicações de desempenho se usado negligentemente em loops ou com strings muito grandes.

## Veja Também:

- [Ruby Doc sobre Strings](https://ruby-doc.org/core-2.7.0/String.html)