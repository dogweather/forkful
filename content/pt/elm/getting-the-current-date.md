---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Aprenda a Obter a Data Atual em Elm

## O Que & Por Quê?

Obter a data atual em programação é o ato de recuperar a data e a hora no momento em que o código é executado. Programadores fazem isso frequentemente quando precisam marcar eventos, registrar informações ou realizar tarefas baseadas em tempo.

## Como Fazer:

Não há um jeito direto de pegar a data atual puramente em Elm, pois isso vai contra sua natureza funcional e previsível. No entanto, você pode usar um `Task` para retornar o tempo atual em milissegundos desde a Época Unix. 

```Elm
import Task exposing (Task)
import Time exposing (Posix, toMillis, utc)
import Task.Perform as Perform

GetTime : Task.Task x Posix
GetTime =
    Time.now

main =
    Perform.task GetTime
```
E ao executar o código acima, você terá um valor que representa os milissegundos desde a Época Unix.

## Mergulhando Fundo

Não há uma função inerente em Elm para obter a data atual como há em outras linguagens devido à sua orientação funcional onde todas as funções devem ser determinísticas. Nesta alternativa, estamos usando a função `Time.now` que retorna um `Task`. 

A Época Unix, mencionada anteriormente, é o número de milissegundos que se passaram desde 1º de janeiro de 1970. A maioria das linguagens de programação baseia-se nesta Época para calcular datas e horários.

Note que a função `Time.now` pode produzir resultados diferentes cada vez que é chamada, quebrando a garantia fundamental de Elm que funções com o mesmo input produzirão sempre o mesmo output. É por isso que `Time.now` retorna um `Task` ao invés de um valor direto.

Lembre-se que lidar com datas e horas em programação pode ser complicado devido a fatores como fusos horários e horário de verão. Portanto, sempre use bibliotecas confiáveis e testadas ao lidar com datas e horas em um aplicativo real.

## Veja Também

1. Documentação oficial Elm: [https://elm-lang.org/docs](https://elm-lang.org/docs)