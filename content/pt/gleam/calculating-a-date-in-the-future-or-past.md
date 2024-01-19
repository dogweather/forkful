---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Gleam: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?  

Calcular uma data no futuro ou passado é um processo de adicionar ou subtrair dias a uma data existente. Os programadores fazem isso para manipular cronogramas, organizar eventos futuros ou analisar dados históricos.

## Como Fazer:  

O Gleam torna fácil trabalhar com datas. Aqui está um exemplo que subtrai dias de uma data:

```gleam
import gleam/otp.{calendar, date}

fn main() {
  let hoje = calendar.local_today()
  let uma_semana_atras = date.subtract(hoje, date.days(7))
  uma_semana_atras
}
```

E o resultado:

```s
# (2022, 2, 10)
```

E para adicionar dias a uma data:

```gleam
fn main() {
  let hoje = calendar.local_today()
  let uma_semana_depois = date.add(hoje, date.days(7))
  uma_semana_depois
}
```
## Mergulho Profundo

Historicamente, calcular a data do futuro ou passado é um desafio. Existem várias formas e calendários para marcar o tempo. E, ao lidar com datas, devemos levar em conta os anos bissextos.

Outras maneiras de calcular datas envolvem o uso de bibliotecas, como a `date-fns` no JavaScript. Porém, o Gleam tem um bom suporte para isso na biblioteca standard OTP.

Internamente, ao adicionar ou subtrair dias a uma data, o Gleam converte a data para uma estrutura de timestamp, realiza a operação, e então converte de volta para o formato de data. Isso permite que o Gleam manipule datas de maneira precisa e eficiente.

## Veja Mais:

- Documentação Oficial do Gleam: https://gleam.run/docs/
- Calendários e a Sua História: http://www.webexhibits.org/calendars/
- Biblioteca JavaScript date-fns: https://date-fns.org/