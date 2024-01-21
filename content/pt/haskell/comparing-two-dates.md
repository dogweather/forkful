---
title:                "Comparando duas datas"
date:                  2024-01-20T17:33:05.194546-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparando duas datas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Comparar duas datas significa verificar a relação temporal entre elas: qual é anterior, posterior ou se são a mesma. Programadores fazem isso para organizar eventos, validar períodos, gerenciar agendas ou qualquer tarefa que dependa de ordem cronológica.

## Como Fazer:
No Haskell, você pode usar a biblioteca `Data.Time` para trabalhar com datas. Aqui está um exemplo simples:

```haskell
import Data.Time

compareDates :: IO ()
compareDates = do
    let date1 = fromGregorian 2023 3 14 -- 14 de Março de 2023
    let date2 = fromGregorian 2023 10 31 -- 31 de Outubro de 2023
    putStrLn $ "Comparing " ++ show date1 ++ " and " ++ show date2
    case compare date1 date2 of
        LT -> putStrLn "A primeira data é mais antiga."
        GT -> putStrLn "A primeira data é mais recente."
        EQ -> putStrLn "As datas são iguais."

main :: IO ()
main = compareDates
```

Saída de amostra:

```
Comparing 2023-03-14 and 2023-10-31
A primeira data é mais antiga.
```

## Imersão Profunda:
Comparar datas é um conceito tão antigo quanto a própria noção de tempo. Em Haskell, a forma de representar datas e horários foi estandardizada com a `Data.Time` na versão 6.12.1 do GHC (Glasgow Haskell Compiler) em 2009. Antes disso, as bibliotecas podiam variar e não havia um padrão claro.

As alternativas incluem escrever sua própria função de comparação (não recomendado, já que tratar casos como anos bissextos pode ser complicado), ou usar bibliotecas antigas, como `old-time` (também não recomendado por ser obsoleta). A biblioteca `Data.Time.Calendar` provê `fromGregorian` para criar datas, e o tipo `Day` suporta diretamente as operações de comparação através da classe `Ord`.

Na implementação, a comparação de datas é geralmente feita convertendo ambas para um formato numérico consistente, como a contagem de dias desde uma data de referência, e então comparando esses números.

## Veja Também:
- [Hackage - Data.Time](https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html): documentação oficial da biblioteca `Data.Time`.
- [GHC User’s Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/): guia do usuário GHC, que tem detalhes sobre as versões do compilador e bibliotecas inclusas.
- [Haskell.org](https://www.haskell.org/): o site oficial de Haskell, contendo muitos recursos, tutoriais e documentações para aprofundar conhecimentos na linguagem.
- [Learn You a Haskell for Great Good](http://learnyouahaskell.com/): um guia divertido e informativo para aprender Haskell.