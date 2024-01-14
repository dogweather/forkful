---
title:                "Haskell: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Por que

Converter uma data em uma string é uma tarefa importante para qualquer programa de computador que precise lidar com datas. Com a conversão, é possível mostrar a data de maneira mais legível para o usuário ou realizar operações como ordenação e comparação.

# Como fazer

A linguagem de programação Haskell fornece uma biblioteca padrão chamada "Data.Time" que inclui funções e tipos de dados para manipular datas. Para converter uma data em uma string, utilizamos a função "show" que recebe como parâmetro um tipo de dados "Day" (dia) e retorna uma string no formato "AAAA-MM-DD".

```Haskell
import Data.Time

main = do
    let dataAtual = fromGregorian 2021 10 10 -- cria uma data com o formato ano-mês-dia
    putStr "Data atual no formato string: "
    putStrLn $ show dataAtual -- converte a data em uma string e imprime na tela
```

**Saída:**

Data atual no formato string: 2021-10-10

Além disso, é possível utilizar a função "formatTime" para personalizar o formato da string de saída. Essa função recebe como parâmetros um string de formatação e um tipo de dados "Day" e retorna uma string com a data formatada. Abaixo temos um exemplo de código que formata a data no formato "dd/MM/aaaa".

```Haskell
import Data.Time.Format
import System.Locale

main = do
    let dataAtual = fromGregorian 2021 10 10
    putStr "Data atual no formato string: "
    putStrLn $ formatTime defaultTimeLocale "%d/%m/%Y" dataAtual
```

**Saída:**

Data atual no formato string: 10/10/2021

# Profundidade

Além dos formatos de data mais comuns, a biblioteca "Data.Time" também fornece funções para converter uma data em uma string no formato ISO 8601 (padrão internacional para representação de datas). Esse formato é útil para comunicação entre diferentes sistemas, pois é facilmente interpretado por todos.

```Haskell
import Data.Time.Format.ISO8601

main = do
    let dataAtual = fromDate (fromGregorian 2021 10 10) -- cria uma data a partir dos componentes ano, mês e dia
    putStr "Data atual no formato ISO 8601: "
    putStrLn $ iso8601Show dataAtual
```

**Saída:**

Data atual no formato ISO 8601: 2021-10-10

Além disso, a biblioteca "Data.Time" também possui funções para converter uma data em uma string utilizando diferentes fuso horários, como "utc" e "local". É importante ter em mente essas variações ao lidar com datas em sistemas que podem estar em diferentes regiões do mundo.

# Veja também

- [Documentação da biblioteca Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial sobre manipulação de datas em Haskell](https://www.learnyouahaskell.com/databases-and-serialization#using-a-custom-date-type)
- [Artigo sobre o padrão ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)