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

# Calculando Datas no Futuro e no Passado com o Shell Fish

## O que & Por quê?
Calculando datas futuras ou passadas é um método que permite ao programador determinar um ponto específico em tempo relativo à data atual. Os programadores fazem isso para rastrear prazos, eventos futuros ou calcular o tempo transcorrido.

## Como fazer

No Shell Fish, utilize a função `date` para manipular datas. Para calcular uma data futura ou passada, o formato é `date -d"+n days"`.

```Fish Shell
date -d"+7 days"
```

A saída será a data de uma semana a partir de hoje.

```Fish Shell
date -d"-30 days"
```

A saída será a data 30 dias atrás.

## Mergulho Profundo

Observe que, usando `date -d`, estamos utilizando a função date incorporada em muitos sistemas operacionais Unix e Linux. Sua utilização remonta à década de 1970, um testemunho de sua utilidade e poder.

Existem alternativas, como a função `strftime` em Python ou a biblioteca `Date` em JavaScript, mas a maioria das linguagens possui suas próprias funções para lidar com datas e tempo.

A data calculada inclui informações como ano, mês, dia, horas, minutos, segundos e fusos horários. O Fish Shell apenas faz uma ponte para a função `date`, portanto, a implementação exata pode variar dependendo do sistema operacional.

## Veja Também

Para mais informações sobre a função `date`, veja [a página de manual](https://man7.org/linux/man-pages/man1/date.1.html). Para saber mais sobre o cálculo e manipulação de datas em geral, aqui estão alguns recursos úteis:

- [Trabalhando com datas e hora no Python](https://docs.python.org/3/library/datetime.html)
- [Date and time in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Como trabalhar com datas e horas no Shell Fish](https://fishshell.com/docs/3.1/tutorial.html#date-and-time)