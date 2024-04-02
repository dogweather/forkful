---
date: 2024-01-20 17:31:18.715907-07:00
description: "Calcular uma data no futuro ou passado \xE9 simplesmente determinar\
  \ uma data adicionando ou subtraindo um certo per\xEDodo de tempo a partir de hoje.\u2026"
lastmod: '2024-03-13T22:44:46.638444-06:00'
model: gpt-4-1106-preview
summary: "Calcular uma data no futuro ou passado \xE9 simplesmente determinar uma\
  \ data adicionando ou subtraindo um certo per\xEDodo de tempo a partir de hoje.\u2026"
title: Calculando uma data no futuro ou passado
weight: 26
---

## O Quê & Porquê?
Calcular uma data no futuro ou passado é simplesmente determinar uma data adicionando ou subtraindo um certo período de tempo a partir de hoje. Programadores fazem isso para lidar com prazos, agendar eventos futuros ou verificar datas passadas em aplicações de todos os tipos.

## Como Fazer:
Vamos usar a biblioteca `time` para manipular datas. Aqui estão os passos e um exemplo simples.

```Haskell
import Data.Time

-- Cálculo de datas futuras (exemplo: daqui a 10 dias)
calcFutureDate :: IO ()
calcFutureDate = do
  today <- getCurrentTime
  let tenDays = addDays 10 (utctDay today)
  putStrLn $ "Daqui a 10 dias será: " ++ show tenDays

-- Cálculo de datas passadas (exemplo: 10 dias atrás)
calcPastDate :: IO ()
calcPastDate = do
  today <- getCurrentTime
  let tenDaysAgo = addDays (-10) (utctDay today)
  putStrLn $ "Há 10 dias foi: " ++ show tenDaysAgo
```

Executar `calcFutureDate` e `calcPastDate` vai te dar a saída com as datas de 10 dias no futuro e no passado a partir de hoje.

## Mergulho Profundo:
O cálculo de datas é uma necessidade comum de longa data na programação. O Haskell, sendo uma linguagem funcional, oferece um tratamento preciso e tipado para datas e tempos. Bibliotecas como `time` são essenciais. Antes, linguagens como C lidavam com datas usando tipos de estruturas mais primitivos, como `time_t`.

Alternativas no Haskell incluem pacotes como `chronos` e `thyme` que oferecem abstrações diferentes para o tempo. Cada um tem seus prós e contras, como desempenho ou facilidade de uso.

Quanto à implementação, a função `addDays` lida com os detalhes de calendário, como anos bissextos e tamanho dos meses. Essas funções garantem que, independente do valor que você adicione ou subtraia, o resultado estará correto nesses termos.

## Veja Também:
- Documentação da biblioteca `time`: http://hackage.haskell.org/package/time
- Tutorial de Haskell oficial (em inglês): https://www.haskell.org/tutorial/
- Página sobre o pacote `chronos`: http://hackage.haskell.org/package/chronos
- Página sobre o pacote `thyme`: http://hackage.haskell.org/package/thyme
