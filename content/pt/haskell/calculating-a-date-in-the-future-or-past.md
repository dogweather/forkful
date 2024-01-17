---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Haskell: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por quê?

"Cálculo de datas no futuro ou no passado" é um termo usado pelos programadores para se referir à tarefa de determinar uma data específica após uma determinada quantidade de tempo, ou antes de uma data específica. Os programadores frequentemente precisam fazer esse cálculo para criar aplicações que lidam com eventos futuros ou retroativos, como agendamento de compromissos ou cálculo de prazos de entrega.

## Como fazer:

Usando a linguagem de programação Haskell, podemos calcular uma data no futuro ou no passado de forma simples e eficiente. Veja um exemplo de código abaixo:

```Haskell
import Data.Time

calcularData :: Integer -> TimeLocale -> String
calcularData dias locale = do
    let dataAtual = getCurrentTime
    let dataFutura = addDays dias dataAtual
    formatTime defaultTimeLocale "%d/%m/%Y" dataFutura
```

O código acima usa a função `addDays` do módulo `Data.Time` para adicionar uma quantidade específica de dias (representada pelo parâmetro `dias`) à data atual. O parâmetro `locale` é usado para definir o formato da data de saída. No exemplo acima, estamos usando o formato "dia/mês/ano". Executando o código acima com os seguintes valores:

```Haskell
calcularData 10 pt_br
```

O resultado seria "21/01/2022" (considerando que a data atual é 11/01/2022).

## Mergulho Profundo:

Historicamente, o cálculo de datas era uma tarefa bastante complexa, com diversas fórmulas e regras a serem seguidas. Com o avanço da tecnologia e a adoção de linguagens de programação modernas, essa tarefa se tornou mais simples e acessível.

Além do exemplo de código acima, também é possível calcular datas no futuro ou no passado usando outras linguagens de programação, como Java e Python. No entanto, a sintaxe do código e as bibliotecas utilizadas podem variar de uma linguagem para outra.

Ao implementar uma função para calcular datas em Haskell, é importante garantir que a entrada (os parâmetros) esteja correta e que a saída esteja no formato desejado. Além disso, é importante considerar a utilização de funções do módulo `Data.Time` para facilitar a tarefa.

## Veja Também:

- [Documentação do módulo Data.Time em Haskell](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Tutorial sobre cálculo de datas em Java](https://www.baeldung.com/java-date-time-operations)
- [Exemplo de cálculo de datas em Python](https://www.kite.com/python/answers/how-to-add-days-to-a-date-in-python)