---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Obter a data atual é uma ação programática para se descobrir a data do sistema em tempo real. Esta técnica é útil para uma multitude de aplicações, como marcação de eventos de log, clima em tempo real em jogos, ou para simplesmente mostrar a data atual para um usuário.

## Como fazer:

Em Haskell, obter a data atual é feito com poucas linhas de código utilizando a biblioteca `Data.Time.Clock`. Aqui vai um exemplo:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
  atual <- getCurrentTime
  print $ utctDay atual
```
Ao executar este código, verá algo assim:

```Haskell
2022-07-29
```

## Mergulho Profundo:

Historicamente, obter a hora e a data do sistema foi algo desafiador desde o princípio da programação. Hoje em dia, bibliotecas como `Data.Time.Clock` em Haskell o tornaram simples e direto.

Existem alternativas para obter a data atual em Haskell, como a biblioteca `System.Time`, mas ela está depreciada e não é recomendada para casos de uso modernos.

A implementação de `getCurrentTime` em Haskell usa bindings para a função C `time()`, que retorna o tempo atual. É uma chamada simples, mas que representa a evolução de muitos anos de programação.

## Veja Também:

Há várias fontes disponíveis se você estiver interessado em aprofundar mais no uso de `Data.Time.Clock` ou outras maneiras de lidar com as horas em Haskell:

* Documentação oficial de `Data.Time.Clock`: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Clock.html
* Perguntas e respostas no StackOverflow sobre `Data.Time`: https://stackoverflow.com/questions/tagged/haskell+datetime
* O livro "Real World Haskell" tem um ótimo capítulo sobre isso: https://book.realworldhaskell.org/read/using-typeclasses.html

Lembre-se, a melhor maneira de aprender é fazendo, então continue escrevendo código e fazendo perguntas.