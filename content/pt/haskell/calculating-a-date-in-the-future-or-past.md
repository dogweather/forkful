---
title:    "Haskell: Calculando uma data no futuro ou passado."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou passado?

Há diversas situações em que pode ser necessário calcular uma data no futuro ou passado, como por exemplo em aplicações de planejamento ou rastreamento de prazos. Essa tarefa pode ser facilitada com o uso de programação funcional em Haskell.

## Como fazer?

Para calcular uma data no futuro ou passado em Haskell, podemos utilizar a função `addDays` da biblioteca `Data.Time`. Essa função recebe como argumento um número inteiro representando a quantidade de dias a serem adicionados ou subtraídos da data atual.

```
import Data.Time

-- Calculando a data daqui a 2 dias
date1 = addDays 2 getCurrentTime

-- Calculando a data de ontem
date2 = addDays (-1) getCurrentTime
```

Podemos também especificar uma data específica como ponto de partida, utilizando a função `fromGregorian` para criar uma data a partir de um ano, mês e dia específicos.

```
date = addDays 10 (fromGregorian 2021 3 20)
```

O resultado do código acima seria a data 30/03/2021.

## Mergulho Profundo

Além da função `addDays`, a biblioteca `Data.Time` possui outras funções que possibilitam manipular datas, como `addMonths`, `addYears`, `addUTCTime`, entre outras. Essas funções podem ser combinadas para realizar cálculos mais complexos com datas.

Porém, é importante lembrar que a biblioteca `Data.Time` trabalha com anos bissextos e datas gregorianas. Caso sua aplicação necessite de funcionalidades mais avançadas, é recomendado utilizar bibliotecas externas que suportem outras convenções de calendário.

## Veja também

- [Documentação da biblioteca `Data.Time`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Tutorial de Haskell da "Learn You a Haskell for Great Good!"](https://learnyouahaskell.com/)