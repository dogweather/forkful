---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Removendo caracteres correspondentes a um padrão em Gleam

## O que & Por quê?

Deletar caracteres correspondentes a um padrão é o processo de localizar e remover partes específicas em uma string. Programadores fazem isso para limpar ou formatar dados, tornando-os mais úteis ou legíveis.

## Como fazer:

Aqui está um exemplo de como você pode deletar caracteres correspondentes a um padrão em Gleam.

```Gleam
import gleam/string

let minha_string = "Olá, mundo!"
let novo = string.replace(minha_string, ",", "")
println(novo) // output: Olá mundo!
```

Nesse exemplo, todas as ocorrências de `,` na string são removidas com a função `string.replace`. 

## Deep Dive

A remoção de caracteres seguindo um padrão tem sido uma prática comum desde os primeiros dias da programação. Nomes de espaços reservados e expressões regulares desempenham um papel importante neste processo.

Existem várias alternativas para remover caracteres. Para padrões simples, `string.replace` é suficiente. Para padrões complexos, `regex.replace` vem a calhar.

Em termos de implementação, Gleam baseia-se na biblioteca Erlang's string, fornecendo uma camada de abstração puramente funcional. Isso garante uma coerência na manipulação de strings em toda a linguagem, independentemente da complexidade do padrão.

## Veja também

Para mais informações sobre programação em Gleam e manipulação de strings, visite esses links:

3. Biblioteca de strings do Erlang: [Erlang String](http://erlang.org/doc/man/string.html)