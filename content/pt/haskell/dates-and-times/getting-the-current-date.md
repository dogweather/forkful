---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:32.341210-07:00
description: "Obter a data atual em Haskell envolve obter o tempo atual do sistema\
  \ e transform\xE1-lo em um formato de data leg\xEDvel. Os programadores fazem isso\
  \ para\u2026"
lastmod: '2024-03-13T22:44:46.635619-06:00'
model: gpt-4-0125-preview
summary: "Obter a data atual em Haskell envolve obter o tempo atual do sistema e transform\xE1\
  -lo em um formato de data leg\xEDvel. Os programadores fazem isso para\u2026"
title: Obtendo a data atual
weight: 29
---

## O Que & Por Quê?
Obter a data atual em Haskell envolve obter o tempo atual do sistema e transformá-lo em um formato de data legível. Os programadores fazem isso para realizar operações baseadas na data, como logging, agendar tarefas ou marcar eventos em aplicações com timestamp.

## Como fazer:
A biblioteca padrão do Haskell, `base`, fornece o módulo `Data.Time`, que oferece funcionalidades para trabalhar com datas e horas. Veja como usá-lo para obter a data atual:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Saída de exemplo:
```
2023-04-12
```

Para mais flexibilidade, como formatar a data ou trabalhar com diferentes fusos horários, a biblioteca `time` é inestimável. Aqui está como você pode formatar a data atual:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

Isso imprime a data atual no formato `YYYY-MM-DD`, ajustado ao fuso horário local.

Adicionalmente, para suporte de bibliotecas de terceiros, `time` é altamente recomendado e frequentemente usado dentro da comunidade Haskell por suas extensas capacidades de manipulação de datas e horas. Os exemplos acima utilizam esta biblioteca.

Se você precisar de manipulação de datas mais abrangente, incluindo análise de strings ou operações aritméticas com datas e horas, explorar funções adicionais dentro de `Data.Time` será benéfico.
