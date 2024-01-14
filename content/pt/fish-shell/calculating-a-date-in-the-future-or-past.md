---
title:                "Fish Shell: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado

Algumas vezes, durante a programação em Fish Shell, podemos precisar calcular uma data no futuro ou passado para determinadas tarefas ou lógicas de negócio. Isso é especialmente útil em scripts ou programas que precisam lidar com datas, como agendamentos de tarefas ou cálculos de juros.

## Como fazer

Para calcular uma data no futuro ou passado no Fish Shell, utilizamos o comando `date`. Por exemplo, para calcular a data de 3 dias no futuro a partir do dia de hoje, usamos o seguinte código:

```Fish Shell
date -d "3 days"
```

Isso nos retornará a data em formato padrão, como por exemplo `Sat Sep 25 16:49:39 2021`. Também é possível modificar o formato de saída, adicionando a opção `-I` seguida de um formato especificado. Por exemplo, para obter a data no formato `YYYY-MM-DD`, usamos o seguinte código:

```Fish Shell
date -d "3 days" -I "%Y-%m-%d"
```

Isso nos retornará `2021-09-25`, que é a data de hoje mais 3 dias no futuro.

## Mergulho profundo

O comando `date` no Fish Shell oferece uma série de opções de configuração para calcular datas futuras ou passadas. Além de especificar o número de dias, podemos também adicionar ou subtrair dias, semanas, meses e anos. Além disso, podemos especificar uma data de referência para calcular a data desejada.

Outra opção útil é a capacidade de calcular datas em um fuso horário diferente, usando a opção `-u` seguida de um fuso horário especificado.

## Veja também

- Documentação oficial do comando `date` no Fish Shell: https://fishshell.com/docs/current/cmds/date.html 
- Tutorial sobre cálculo de datas no Fish Shell: https://afontainhas.com/posts/calculating-time-in-fish-shell/