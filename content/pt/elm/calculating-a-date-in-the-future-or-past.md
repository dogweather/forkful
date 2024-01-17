---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Elm: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que é e por quê?
Calcular uma data no futuro ou no passado é a ação de criar uma nova data com base em uma data existente e um número de dias adicionados ou subtraídos. Programadores geralmente realizam essa tarefa para criar recursos como uma agenda, um cronograma ou cálculos financeiros.

## Como fazer:
````elm
import Time exposing (..)

futureDate : Int -> Date -> Date
futureDate days date =
    date
        |> toTime
        |> Time.add (Time.days days)
        |> toDate

pastDate : Int -> Date -> Date
pastDate days date =
    date
        |> toTime
        |> Time.sub (Time.days days)
        |> toDate

today : Date
today =
    Date.fromTime (Time.now)

elmDates : String
elmDates =
    "Hoje é " ++ toString today ++ ", e daqui a 10 dias será " ++ toString (futureDate 10 today) ++ "."

putStrLn elmDates -- Hoje é 2020-01-15, e daqui a 10 dias será 2020-01-25.
````
## Mergulho Profundo:
Calcular datas no futuro ou no passado tem sido uma tarefa importante ao longo da história da computação, desde o desenvolvimento de calendários precisos até programas de agendamento. Uma alternativa para calcular datas no futuro ou no passado é usar bibliotecas de terceiros que podem fornecer uma funcionalidade mais abrangente. A implementação do cálculo pode depender do tipo de data, incluindo dias bissextos e fusos horários.

## Veja também:
- Documentação de Time em Elm: <https://package.elm-lang.org/packages/elm/time/latest>
- Biblioteca Date in Elm: <https://package.elm-lang.org/packages/elm-community/date-extra/latest/>