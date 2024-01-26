---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:36:43.185321-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Analisar datas de texto permite converter representações de data e hora em formatos legíveis por máquinas para uso posterior, como armazenamento, comparação ou manipulação. Fazemos isso porque os programas frequentemente precisam entender as datas que os usuários inserem como strings em formatos variados.

## Como fazer:
```Haskell
import Data.Time

-- Considerando que você tem a biblioteca time instalada
-- Faça o parsing de uma data no formato ISO 8601
parseIsoDate :: String -> Maybe Day
parseIsoDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Exemplo de uso e saída esperada
main :: IO ()
main = do
    let maybeDate = parseIsoDate "2023-03-14"
    case maybeDate of
        Just date -> print date  -- Saída: 2023-03-14
        Nothing -> putStrLn "Data inválida."
```

## Aprofundamento
O parsing de datas em Haskell tem como aliado a biblioteca 'time', que é madura e amplamente utilizada. Historicamente, a manipulação de datas e horas em computadores é desafiadora devido a considerações sobre fusos horários, calendários e formatos locais. Alternativas incluem bibliotecas como 'chronos' e 'thyme', mas 'time' permanece popular pelo seu suporte nativo e extensiva documentação.

Detalhes de implementação normalmente giram em torno da função `parseTimeM`, que usa um locale (no exemplo, `defaultTimeLocale`), um formato de data (como "%Y-%m-%d" para ISO 8601) e a string a ser analisada. A função retorna um `Maybe Day`, onde `Just day` contém a data em caso de sucesso, e `Nothing` indica um erro de análise.

## Veja Também
- [Documentação da biblioteca time](https://hackage.haskell.org/package/time-1.12/docs/Data-Time.html)
- [SO: Parsing a date in Haskell](https://stackoverflow.com/questions/4426948/parsing-a-date-in-haskell)
