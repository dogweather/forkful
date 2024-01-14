---
title:    "Haskell: Comparando duas datas"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Haskell?

Comparar datas é uma tarefa comum em programação, especialmente quando se trabalha com informações relacionadas a horários e agendamentos. Em Haskell, uma linguagem funcional e puramente funcional, há várias maneiras de se comparar duas datas. Neste post, vamos explorar como fazer isso de maneira eficiente e elegante utilizando recursos da linguagem.

## Como comparar duas datas em Haskell

Para comparar duas datas em Haskell, podemos utilizar o módulo `Data.Time` que fornece funções e tipos para representar e manipular datas. Para este exemplo, vamos criar duas variáveis com datas diferentes e compará-las para determinar qual delas é maior.

```Haskell
import Data.Time

data1 :: Day
data1 = fromGregorian 2020 2 15

data2 :: Day
data2 = fromGregorian 2020 4 20

if data1 > data2 then putStrLn "A primeira data é maior"
else putStrLn "A segunda data é maior"
```

Neste código, utilizamos a função `fromGregorian` para criar uma data no formato ano-mês-dia. Em seguida, comparamos as duas datas utilizando o operador `>`, que verifica se a primeira data é maior que a segunda. No caso, a saída será "A segunda data é maior".

## Aprofundando na comparação de datas

É importante ressaltar que em Haskell, para comparar datas é necessário utilizar tipos específicos, como `Day` ou `UTCTime`, e não é possível utilizar o operador `==` para verificar igualdade. Além disso, também é possível utilizar funções como `diffDays`, que retorna a diferença em dias entre duas datas, ou `addDays` que adiciona uma determinada quantidade de dias a uma data.

Se precisarmos trabalhar com datas e horas mais precisas, podemos utilizar o módulo `Data.Time.Clock`, que fornece uma resolução maior do que o módulo `Data.Time`.

## Veja também

- [Documentação do módulo Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Função fromGregorian](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#v:fromGregorian)
- [Função addDays](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html#v:addDays)