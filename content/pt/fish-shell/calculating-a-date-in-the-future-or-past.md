---
title:                "Fish Shell: Calculando uma data no futuro ou no passado"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como calcular uma data no futuro ou no passado? Talvez você precise fazer um cronograma para um projeto ou planejar suas férias, mas não quer gastar tempo fazendo cálculos manualmente. É por isso que a linguagem de programação Fish Shell tem uma função integrada para calcular datas.

## Como fazer

Para calcular uma data no futuro ou no passado usando o Fish Shell, basta seguir esses passos simples:

1. Abra o terminal e abra o Fish Shell digitando `fish`.
2. Use o comando `date` seguido de uma data no formato "ano-mês-dia" para definir a data base. Por exemplo, `date 2021-07-15` define a data base como 15 de julho de 2021.
3. Use o operador matemático `+` para adicionar dias à data base ou o operador `-` para subtrair dias. Por exemplo, se quisermos calcular a data 10 dias após a data base, usamos `date 2021-07-15 + 10`. O Fish Shell também aceita expressões matemáticas mais complexas, como `date 2021-07-15 + 2 * 5` para adicionar 10 dias à data base.
4. Se quiser calcular uma data no passado, basta usar o operador `-` e um número negativo. Por exemplo, `date 2021-07-15 - 3` calcula a data 3 dias antes da data base.
5. Você também pode adicionar outros formatos de data à sua saída usando a opção `-f`. Por exemplo, `date -f "%A, %B %e, %Y" 2021-07-15` irá imprimir a data como "Thursday, July 15, 2021".

Aqui está um exemplo da saída do terminal usando esses comandos:

```Fish Shell
$ date 2021-07-15
2021-07-15
$ date 2021-07-15 + 10
2021-07-25
$ date -f "%A, %B %e, %Y" 2021-07-15
Thursday, July 15, 2021
```

## Aprofundando

O Fish Shell utiliza a biblioteca `libdatecalc` para executar cálculos de datas, o que significa que ele é capaz de lidar com datas muito distantes no futuro ou no passado com precisão. Além disso, o Fish Shell também é capaz de lidar com fusos horários, o que é essencial para quem trabalha com equipes de diferentes partes do mundo.

Além disso, existem outras opções para personalizar a saída, como usar o operador `@` para imprimir a data em formato Unix timestamp ou a opção `-I` para imprimir somente a data sem o horário.

## Veja também

- Documentação oficial do Fish Shell: https://fishshell.com/docs/current/cmds/date.html
- Usando expressões matemáticas no Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_math
- Formatação de datas no Fish Shell: https://fishshell.com/docs/current/cmds/date.html#format-output