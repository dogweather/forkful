---
title:                "Comparando duas datas"
html_title:           "Haskell: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador em Haskell, provavelmente já precisou comparar duas datas em algum momento de seu desenvolvimento. Comparar duas datas é útil para diversas aplicações, como por exemplo, verificar se uma data está antes ou depois de outra, calcular a diferença entre duas datas ou verificar se uma data é igual a outra.

## Como Fazer

Comparar duas datas em Haskell é bastante simples e podemos utilizar algumas funções básicas da linguagem para isso. Vamos ver alguns exemplos:

```
-- Verificar se uma data está antes de outra
compareDates :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
compareDates (d1, m1, y1) (d2, m2, y2) = 
  if y1 == y2
    then if m1 == m2
      then if d1 < d2
        then True
        else False
      else if m1 < m2
        then True
        else False
    else if y1 < y2
      then True
      else False

-- Verificar se uma data é igual a outra
checkIfEqual :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
checkIfEqual date1 date2 = date1 == date2

-- Calcular a diferença em dias entre duas datas
diffDates :: (Int, Int, Int) -> (Int, Int, Int) -> Int
diffDates (d1, m1, y1) (d2, m2, y2) = abs (dateToDays (d1, m1, y1) - dateToDays (d2, m2, y2)) where
  dateToDays :: (Int, Int, Int) -> Int
  dateToDays (date, month, year) = year * 365 + month * 30 + date
```

Aqui utilizamos a função `compareDates` para verificar se uma data está antes de outra, a função `checkIfEqual` para verificar se duas datas são iguais e a função `diffDates` para calcular a diferença em dias entre duas datas. Podemos utilizar essas funções em nosso código para realizar comparações de datas de forma rápida e eficiente.

## Deep Dive

Comparar duas datas em Haskell pode parecer simples, mas existem algumas coisas que devemos ter em mente. Primeiramente, é importante ressaltar que datas são apenas tuplas de três números inteiros, e Haskell permite que utilizemos pattern matching para extrair esses valores facilmente.

Além disso, é importante trabalharmos com as datas de forma consistente, ou seja, sempre utilizar o formato (d, m, y) para representá-las, onde d é o dia, m é o mês e y é o ano. Isso nos ajuda a evitar erros de comparação e cálculos incorretos.

Outro ponto importante é a utilização da função `abs` para calcular a diferença em dias entre duas datas. Isso é necessário porque, caso a primeira data seja maior do que a segunda, o resultado será um valor negativo, o que pode gerar erro em nossos códigos caso não tratemos esse valor corretamente.

Com essas dicas em mente, você estará pronto para utilizar a comparação de datas em seu código Haskell de forma eficiente e sem complicações.

## Veja também

- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell para iniciantes](https://learnxinyminutes.com/docs/pt-br/haskell-pt/)
- [Exemplos práticos de comparação de datas em Haskell](https://wiki.haskell.org/Comparing_dates_and_times)