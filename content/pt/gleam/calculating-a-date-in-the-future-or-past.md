---
title:    "Gleam: Calculando uma data no futuro ou no passado"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que

Existem muitas situações em que você pode precisar calcular uma data no futuro ou no passado em seu código Gleam. Isso pode ser útil em casos como agendamento de tarefas, geração de relatórios ou planejamento de eventos.

## Como fazer

A função `add_duration()` é a chave para calcular datas futuras em Gleam. Você pode especificar o número de dias, semanas, meses ou anos que deseja adicionar à data atual. Aqui está um exemplo de como usá-lo:

```Gleam
import gleam/time

let data_futura = time.add_duration(time.now(), time.Duration(days: 30))
```

Este código irá calcular uma data 30 dias após a data atual e armazená-lo na variável `data_futura`.

Para calcular uma data no passado, você pode usar a função `subtract_duration()`. Aqui está um exemplo:

```Gleam
import gleam/time

let data_passada = time.subtract_duration(time.now(), time.Duration(months: 6))
```

Este código irá calcular uma data 6 meses antes da data atual e armazená-la na variável `data_passada`.

## Aprofundando

Além das funções `add_duration()` e `subtract_duration()`, a biblioteca `gleam/time` oferece outras ferramentas úteis para trabalhar com datas. Você pode explorar mais sobre elas em sua documentação oficial.

Uma coisa importante a notar é que as funções de cálculo de datas em Gleam usam o formato ISO 8601. Isso significa que os meses são representados por números de 1 a 12 e os dias de 1 a 31. Além disso, as funções lidam automaticamente com anos bissextos, garantindo resultados precisos.

## Veja também

- Documentação oficial da biblioteca `gleam/time`
- Tutorial de introdução ao Gleam
- Guia de estilo de código para o Gleam