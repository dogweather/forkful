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

# Manipulação de Datas em Haskell: Como Calcular uma Data Futura ou Passada

## O que & Por quê?
Calcular uma data futura ou passada significa ajustar uma data atual para o futuro ou passado, adicionando ou subtraindo dias, meses ou anos. Os programadores fazem isso para lidar com cenários como datas de validade, cronogramas de projetos e planejamento de eventos.

## Como Fazer:

Haskell fornece o pacote `Data.Time` para lidar com datas e horas. Aqui está um exemplo básico para adicionar e subtrair dias de uma data.

```Haskell
import Data.Time

adicionarDias :: Integer -> Day -> Day
adicionarDias dias = addDays dias

subtrairDias :: Integer -> Day -> Day
subtrairDias dias = addDays (negate dias)

main :: IO ()
main = do
    let today = ModifiedJulianDay 59000
    putStrLn ("Hoje é: " ++ show today)
    putStrLn ("40 dias a partir de agora será: " ++ show (adicionarDias 40 today))
    putStrLn ("30 dias atrás foi: " ++ show (subtrairDias 30 today))
```

Este script mostra a data atual, a data 40 dias a partir de agora e a data 30 dias atrás.

![output.png](http://.../output.png)

## Deep Dive:

Em Haskell, a manipulação de datas é feita usando a biblioteca `Data.Time`, que foi adicionada na versão 1.6 da biblioteca Haskell Platform. Antes disso, os programadores geralmente faziam isso manualmente com funções e listas.

Alternativamente, você pode usar bibliotecas adicionais como `time-lens` para operações mais complexas de manipulação de data e hora, como lidar com zonas de tempo.

Internamente, as datas são contadas como dias a partir de uma "epoch" e armazenadas como `Integer`, tornando adições e subtrações simplificadas. O armazenamento da data como um número inteiro fornece precisão máxima permitindo a manipulação para qualquer data, futuro ou passado.

## Veja Também:

- [Página do módulo Data.Time no Hackage](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Biblioteca time-lens para manipulação de datas e horas](http://hackage.haskell.org/package/time-lens)
- [Documentação da Haskell Platform](https://www.haskell.org/platform/)