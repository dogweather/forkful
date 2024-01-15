---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Fish Shell: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos calcular uma data futura ou passada em nossa programação, por exemplo, para agendar tarefas ou gerar relatórios com base em uma data específica. O Fish Shell possui funções integradas que facilitam esse cálculo, tornando o processo mais eficiente e preciso.

## Como fazer

Para calcular uma data futura ou passada com o Fish Shell, precisamos usar a função `date`. Vamos ver alguns exemplos:

```
Fish Shell: date -d '1 month ago'
Saída: Seg Mar 22 17:16:31 -03 2021
```

Neste exemplo, usamos a opção `-d` para especificar uma data base, ou seja, a partir de qual data queremos calcular. No caso acima, usamos `'1 month ago'` para calcular a data um mês atrás.

Podemos usar outras opções, como `-y` para informar o ano, `-m` para o mês e `-d` para o dia. Veja:

```
Fish Shell: date -d 'yesterday'
Saída: Seg Mar 22 17:16:31 -03 2021
```

Ou:

```
Fish Shell: date -d '2021-04-27 10:30'
Saída: Ter Mar 27 10:30:00 -03 2021
```

Também é possível usar expressões matemáticas, como neste exemplo:

```
Fish Shell: date -d '+3 weeks +2 days'
Saída: Ter Abr 13 17:16:31 -03 2021
```

Assim como podemos calcular uma data no passado, usando valores negativos:

```
Fish Shell: date -d '-5 days'
Saída: Sáb Mar 20 17:16:31 -03 2021
```

## Mergulho profundo

A função `date` no Fish Shell usa o formato de data `yyyyMMddHHmmss`, que é o mesmo formato usado pelo comando `date` nas distribuições Linux. Se quisermos personalizar o formato de saída, podemos usar a opção `-f` seguida de uma string com as instruções de formatação desejadas. Por exemplo:

```
Fish Shell: date -d 'last year' -f '%Y-%m-%d'
Saída: 2020-03-24
```

Com essa opção, podemos formatar a saída como desejarmos, incluindo o dia da semana, horas, minutos, entre outros.

## Veja também
- [Fish Shell: Um guia prático para iniciantes](https://www.datasciencecentral.com/profiles/blogs/fish-shell-um-guia-pratico-para-iniciantes)
- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guia de referência rápida para comandos do Fish Shell](https://devhints.io/fish-shell)