---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Elm: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

---

## O Que & Porquê?

Calcular uma data no futuro ou no passado é o processo de manipular datas para obter um horizonte temporal diferente. Programadores fazem isso para lidar com prazos e manipular informações de tempo nos sistemas que criam.

## Como Fazer:

Vamos usar a biblioteca `elm/time` para manipular datas. Começa com a instalação:

```
elm install elm/time
```

Aqui está um exemplo de como calcular uma data 3 dias à frente:

```Elm
import Time exposing (..)
import Task

adicionaDias : Int -> Posix -> Posix
adicionaDias dias date =
    let
        millisegundos = dias * 24 * 60 * 60 * 1000
    in
    Time.millisToPosix (millisegundos + Time.posixToMillis date)

principal : Task.Task Never ()
principal =
    Task.perform (\_ -> Debug.log "Data Atual + 3 dias: ")
    <| Task.map (adicionaDias 3) Time.now
```

Execute este código e você verá a data atual mais 3 dias na consola.

## Mergulho Profundo

Calcular uma data no futuro ou no passado tem sido uma prática comum desde os primeiros dias da programação. As linguagens contemporâneas, como Elm, possuem bibliotecas ricas como `elm/time` para facilitar as manipulações de data.

Alternativas incluem a criação de suas próprias funções de manipulação de tempo, ou usar bibliotecas de terceiros, se disponíveis. No entanto, a escolha da biblioteca `elm/time`, recomendada pela comunidade Elm, garante a precisão e simplifica as coisas ao máximo.

Os detalhes de implementação passam por gerir tempos como Instantes Posix, um sistema universal para representar pontos no tempo, evitando muitos dos problemas de fuso horário e de verão/inverno.

## Ver Também

1. Documentação da biblioteca Elm Time: https://package.elm-lang.org/packages/elm/time/latest/
2. Recursos de aprendizado do Elm: https://elm-lang.org/docs

Além disso, a comunidade Elm é vibrante e você pode encontrar muitos códigos de exemplo e discussões sobre estes tópicos.

---

Última atualização: Use a versão atual mais recente do Elm para garantir a congruência dos exemplos de código.