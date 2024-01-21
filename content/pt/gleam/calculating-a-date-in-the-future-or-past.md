---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:31:25.742994-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Calcular uma data futura ou passada é determinar uma data específica, adicionando ou subtraindo uma quantidade de tempo a partir de uma data base. Programadores fazem isso para funções como agendar eventos, gerar prazos ou verificar a validade de algum dado temporal.

## Como Fazer:

Primeiro, vamos usar as bibliotecas padrão do Gleam para lidar com datas e tempos. Eis um exemplo simples de como calcular a data de amanhã:

```gleam
import gleam/calendar.{Date, Duration, add_duration}

pub fn main() {
  let hoje = Date(year: 2023, month: 4, day: 23)
  let duracao = Duration(days: 1)
  
  let amanha = hoje
    |> add_duration(duracao)
  
  amanha
}
```

Esse código retorna: `Date(year: 2023, month: 4, day: 24)`

Agora, se quisermos calcular uma data no passado, basta usar um valor negativo:

```gleam
import gleam/calendar.{Date, Duration, add_duration}

pub fn main() {
  let hoje = Date(year: 2023, month: 4, day: 23)
  let duracao = Duration(days: -1)
  
  let ontem = hoje
    |> add_duration(duracao)
  
  ontem
}
```

Esse código retorna: `Date(year: 2023, month: 4, day: 22)`

## Mergulho Profundo

Historicamente, a manipulação de datas sempre foi complexa por causa de diferentes calendários e zonas horárias. O Gleam procura simplificar isso com uma API consistente e funções bem desenhadas.

Existem alternativas diretas como adicionar manualmente os dias ao mês e ajustar os anos, mas isso rapidamente se torna complicado por causa de meses com diferentes quantidades de dias e anos bissextos. Bibliotecas como a `calendar` em Gleam abstraem esses detalhes complicados.

Internamente, calcular uma data futura ou passada em Gleam geralmente se baseia em Epoch time (o número de segundos desde 1 de janeiro de 1970) e operações matemáticas padrão. Contudo, a representação em alto nível através de `Date` e `Duration` mantém o código claro e fácil de ler.

## Veja Também

- Exemplos de código de manipulação de datas em diferentes linguagens: [rosettacode.org/wiki/Dates](https://rosettacode.org/wiki/Date_manipulation)