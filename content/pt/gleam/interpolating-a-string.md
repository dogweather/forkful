---
title:                "Interpolando uma string"
date:                  2024-01-20T17:51:06.863818-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Por Que?
Interpolar uma string é o processo de incorporar variáveis diretamente no texto de uma string. Programadores fazem isso para criar mensagens dinâmicas, personalizadas e para reduzir a complexidade e erros em concatenação de strings.

## Como Fazer:
A interpolação de strings no Gleam é feita com a sintaxe `{}` dentro de strings marcadas com a letra `s` no início. Aqui estão alguns exemplos com a saída esperada:

```gleam
let name = "Mundo"
let message = s"Olá, {name}!"
io.println(message)
```
Saída:
```
Olá, Mundo!
```

Um exemplo com expressões mais complexas:
```gleam
let aircraft = "avião"
let flight_number = 815
let departure = "Lisboa"
let destination = "Porto"

let announcement = s"O {aircraft} número {flight_number} irá de {departure} para {destination}."
io.println(announcement)
```
Saída:
```
O avião número 815 irá de Lisboa para Porto.
```

## Mergulho Profundo
A interpolação de strings não é um conceito novo, existindo em muitas outras linguagens como Python, Ruby e JavaScript. No contexto histórico, ajuda a evitar a confusa e propensa a erros concatenação de strings, onde tínhamos que usar o operador de adição para juntar partes de uma mensagem.

Alternativas à interpolação incluem a concatenação manual (usando `+` ou funções específicas) ou o uso de funções de formatação como `sprintf` em outras linguagens.

Em termos de implementação, o compilador de Gleam geralmente transforma a interpolação de strings em uma sequência de operações de junção de strings, similar à concatenação manual, mas de uma maneira muito mais legível e menos suscetível a erros.

## Veja Também
Vislumbre mais sobre strings em Gleam e outras funcionalidades da linguagem nos links abaixo:

- [The Gleam Book - Strings](https://gleam.run/book/tour/strings.html)