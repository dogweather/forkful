---
date: 2024-01-20 17:36:43.690529-07:00
description: 'Como fazer: Sample output.'
lastmod: '2024-04-05T21:53:46.982386-06:00'
model: gpt-4-1106-preview
summary: ''
title: Convertendo uma data em uma string
weight: 28
---

## Como fazer:
```haskell
import Data.Time

-- Convertendo a data atual em uma string
main :: IO ()
main = do
    -- Pega a data e hora atual
    currentDateTime <- getCurrentTime

    -- Converte a data/hora em uma string formatada (yyyy-mm-dd)
    let dateString = formatTime defaultTimeLocale "%Y-%m-%d" currentDateTime

    -- Mostra a string formatada
    putStrLn dateString
```

Sample output:
```
2023-03-15
```

## Aprofundando
Converter datas em strings está diretamente associado aos primeiros dias da computação quando a manipulação e armazenamento de datas era complicado devido ao formato binário. Com o tempo, surgiram bibliotecas padrão em várias linguagens, como o `Data.Time` em Haskell, para simplificar o processo.

Existem alternativas para formatar datas em Haskell, incluindo bibliotecas externas como `time-fmt` que podem oferecer mais opções personalizadas. No entanto, o `Data.Time` é a escolha padrão por ser amplamente suportado e funcional.

Os detalhes da implementação envolvem o uso de locales (`defaultTimeLocale` no exemplo) que definem a representação de texto da data e hora de acordo com a localidade e a cultura. Isso é útil para internacionalização.

## Veja também
- Haskell 'time' library documentation: [Hackage Time](https://hackage.haskell.org/package/time)
- Time formatting in Haskell: [Formatting Time and Date in Haskell](https://two-wrongs.com/haskell-time-library-tutorial)
