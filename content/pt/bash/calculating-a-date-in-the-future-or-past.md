---
title:                "Bash: Calculando uma data no futuro ou passado"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que Calcular uma Data no Futuro ou Passado?

Calcular datas pode ser uma tarefa útil para várias situações - como planejar eventos, pagamentos ou lembretes. Através da programação em Bash, podemos facilmente automatizar esse processo e economizar tempo e esforço.

## Como Fazer:

Para calcular uma data no futuro ou passado em Bash, podemos usar o comando `date` combinado com a opção `-d` para especificar o intervalo de tempo e a opção `+%Y-%m-%d` para formatar a data. Por exemplo:

```Bash
dataFutura=$(date -d "3 dias" +"%Y-%m-%d")
dataPassada=$(date -d "2 semanas" +"%Y-%m-%d")
```

O comando `date` também pode receber parâmetros mais complexos, como uma data específica ou uma data relativa a outra. Por exemplo:

```Bash
dataEspecifica=$(date -d "2022-01-15")

dataRelativa=$(date -d "1 mês depois de 2021-02-05")
```

Com isso, podemos calcular datas futuras ou passadas com facilidade e precisão.

## Mergulho Profundo:

O comando `date` em Bash também possui outras opções úteis, como o uso de unidades de tempo diferentes - como anos, meses, semanas, dias, horas, minutos e segundos - para especificar o intervalo de tempo. Além disso, podemos usar também opções de formatação diferentes, como `%A` para o dia da semana ou `%B` para o mês por extenso.

Além disso, o Bash possui uma biblioteca de cálculo de datas integrada chamada `mcalc`, que oferece funções matemáticas para trabalhar com datas. Podemos usá-la para adicionar ou subtrair dias, meses ou anos de uma data específica.

## Veja também:

- [Documentação oficial do comando `date` em Bash](https://www.gnu.org/software/coreutils/date)
- [Tutorial sobre manipulação de datas com Bash](https://opensource.com/article/18/9/bash-date-command)
- [Dicas e truques para trabalhar com datas em Bash](https://www.howtogeek.com/441260/how-to-work-with-dates-in-bash-using-the-date-command/)