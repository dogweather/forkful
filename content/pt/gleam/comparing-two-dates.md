---
title:                "Gleam: Comparando duas datas"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas é Útil no Gleam

Comparar datas é uma tarefa comum em muitos projetos de programação. No Gleam, uma linguagem de programação funcional que executa no ambiente de máquina virtual Erlang, também é possível comparar duas datas para determinar qual é a mais recente. Isso pode ser muito útil em casos como encontrar a diferença em dias entre duas datas ou criar uma funcionalidade que valide a data de expiração de um documento. Neste post, vamos explorar como comparar duas datas no Gleam.

## Como Fazer

Abaixo, mostraremos um exemplo simples de como comparar duas datas no Gleam utilizando a função `compare` da biblioteca padrão do Gleam.

```Gleam
import gleam/time

let hoje = time.now()
let amanha = Time.add(hoje, 1, "day")

let diff = Time.compare(hoje, amanha)

IO.print(diff) // 1
```

No exemplo acima, estamos importando a biblioteca `gleam/time` que nos permite trabalhar com datas e tempos no Gleam. Então, definimos duas variáveis `hoje` e `amanha` que representam as datas de hoje e amanhã, respectivamente. Em seguida, usamos a função `compare` passando essas duas datas como argumentos para determinar qual é a mais recente. O valor retornado da função `compare` será um número negativo se a primeira data for mais antiga, zero se as datas forem iguais ou um número positivo se a primeira data for mais recente. No nosso exemplo, o valor retornado é 1, pois a segunda data é um dia depois da primeira data.

Você também pode usar a função `diff_in_days` para encontrar a diferença em dias entre duas datas.

```Gleam
import gleam/time

let agora = time.now()
let mais_tarde = Time.add(agora, 3, "day")

let diff = Time.diff_in_days(agora, mais_tarde)

IO.print(diff) // 3
```

Nesse exemplo, estamos usando a função `diff_in_days` para determinar a diferença em dias entre as duas datas. O valor retornado será 3, já que a segunda data é 3 dias depois da primeira data.

## Profundando

Existem outras funções na biblioteca `gleam/time` que podem ser úteis ao trabalhar com datas no Gleam. Aqui estão algumas delas:

- `add`: adiciona uma quantidade específica de tempo a uma data
- `diff_in_hours`: encontra a diferença em horas entre duas datas
- `is_after`, `is_before`, `is_same` e outras funções para comparar diretamente duas datas

Essas funções, juntamente com a função `compare`, podem ser úteis em diferentes cenários em que é necessário trabalhar com datas.

## Veja Também

- Documentação da Biblioteca de Datas e Tempos do Gleam: https://gleam.run/modules/time.html
- Tutorial do Gleam sobre Trabalhando com Datas e Tempos: https://gleam.run/news/dates-and-times.html