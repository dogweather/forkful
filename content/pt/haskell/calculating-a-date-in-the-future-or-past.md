---
title:                "Haskell: Calculando uma data no futuro ou passado."
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

Calcular datas no futuro ou passado é uma tarefa comum em muitos programas, especialmente em aplicações financeiras e de gerenciamento de projetos. Saber como fazer isso em Haskell pode ajudar os desenvolvedores a tornar seus códigos mais eficientes e precisos.

## Como fazer

Usar Haskell para calcular uma data no futuro ou passado pode ser feito com o uso da biblioteca "time". Primeiro, importe a biblioteca com o comando `import Data.Time`. Em seguida, defina a data base desejada com o tipo `Day`, que representa uma data no calendário gregoriano. Por exemplo, `baseDate = fromGregorian 2021 10 15` define a data base como 15 de outubro de 2021.

Para obter a data no futuro ou passado, é possível usar as funções `addDays` (adiciona dias), `addMonths` (adiciona meses) e `addYears` (adiciona anos). Essas funções retornam uma nova data com o número especificado de dias, meses ou anos adicionados à data base. Por exemplo, `addDays 30 baseDate` retorna a data 30 dias após `baseDate`.

Outra opção é usar a função `addGregorianMonthsClip`, que adiciona o número especificado de meses à data base, garantindo que a data resultante permaneça no formato gregoriano válido. Por exemplo, `addGregorianMonthsClip 12 baseDate` retorna a data 12 meses após `baseDate`.

## Mergulho profundo

Além das funções mencionadas acima, a biblioteca "time" também oferece opções para lidar com datas com diferentes fusos horários e formatos de data. Por exemplo, a função `toGregorian` pode ser usada para converter uma data em formato `Day` para uma tupla no formato (ano, mês, dia). Já a função `parseTimeOrError` pode ser usada para converter uma string em um formato específico de data para o tipo `Day`.

Além disso, é importante verificar a documentação da biblioteca para entender melhor todas as opções disponíveis e como utilizá-las em cada situação específica.

## Veja também

- Documentação da biblioteca "time": https://hackage.haskell.org/package/time
- Tutorial sobre manipulação de datas em Haskell: https://wiki.haskell.org/Handling_time_zones_and_daylight_saving_time
- Exemplos de código em Haskell para cálculo de datas: https://www.programing.fun/calc-date-in-future-haskell-examples