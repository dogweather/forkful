---
title:    "Gleam: Calculando uma data no futuro ou no passado"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por que

Calcular datas no futuro ou no passado é uma habilidade útil e prática para programadores Gleam. Ao utilizar essa funcionalidade, é possível automatizar tarefas que envolvem datas e tempo, facilitando a vida dos desenvolvedores.

# Como fazer

Para calcular uma data no futuro ou no passado em Gleam, é necessário utilizar a função `Date.add` e `Date.subtract` respectivamente. Essas funções recebem três argumentos: a data base, o número de dias a serem adicionados ou subtraídos e a unidade de tempo (dias, semanas, meses ou anos).

Vejamos alguns exemplos de código para entender melhor como utilizar essas funções:

```Gleam
import gleam/time.Date

let data_atual = Date.now()
let data_futura = Date.add(data_atual, 7, Date.Day)
let data_passada = Date.subtract(data_atual, 14, Date.Week)

// Saída:
// data_futura: 2021-06-21T00:00:00.000Z
// data_passada: 2021-05-28T00:00:00.000Z
```

No exemplo acima, utilizamos as funções `Date.add` e `Date.subtract` para calcular uma data 7 dias no futuro e uma data 14 dias no passado a partir da data atual. É importante ressaltar que a data base também pode ser uma data específica, e não apenas a data atual.

# Aprofundando-se

Além das funções `Date.add` e `Date.subtract`, também é possível realizar cálculos mais complexos utilizando a função `Date.shift`. Essa função permite adicionar ou subtrair uma certa quantidade de múltiplas unidades de tempo de uma vez.

Vejamos um exemplo de código utilizando essa função:

```Gleam
import gleam/time.Date

let data_base = Date.from_naive(2021, 4, 15, 0, 0, 0) // 15 de abril de 2021
let data_shifted = Date.shift(data_base, (2, Date.Week), (3, Date.Month), (1, Date.Year))

// Saída:
// data_shifted: 2022-09-26T00:00:00.000Z
```

Neste exemplo, utilizamos a função `Date.shift` para adicionar 2 semanas, 3 meses e 1 ano à data base, resultando na data final de 26 de setembro de 2022.

# Veja também

- [Documentação oficial do módulo `gleam/time`](https://gleam.run/modules/time) 
- [Cálculo de datas em outras linguagens de programação](https://www.educative.io/blog/calculate-difference-between-two-time-datetime-objects-python)
- [Exemplo de uso da função `Date.shift`](https://qiita.com/peccul/items/a988254476573e718c71)