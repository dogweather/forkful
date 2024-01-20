---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:14:58.394027-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Obter a data atual no Haskell significa acessar a data do sistema onde o programa está rodando. Isso é útil para registrar eventos, fazer operações baseadas em datas ou simplesmente exibir a data para o usuário.

## Como Fazer:
```Haskell
import Data.Time

-- Obtendo a data atual
main :: IO ()
main = do
    currentDay <- fmap utctDay getCurrentTime
    print currentDay
```

Saída de exemplo:
```
2023-04-05
```

## Mergulho Profundo
Antigamente em Haskell, acessar a data e a hora era uma tarefa menos direta, exigindo bibliotecas adicionais ou funções mais complexas. Hoje, a biblioteca `Data.Time` facilita esse processo, encapsulando a maioria das funcionalidades que precisamos.

Existem alternativas a `Data.Time`, como `old-time` e `time`, mas `Data.Time` é a biblioteca recomendada hoje em dia por ser mais moderna e ter suporte contínuo.

Quando você chama `getCurrentTime`, o Haskell internamente acessa o relógio do sistema operacional para buscar a hora universal coordenada (UTC). Ao aplicar `utctDay`, convertemos esse valor para apenas a data, ignorando o tempo.

## Veja Também
- Para mais sobre `Data.Time`: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Tutorial sobre datas e horas em Haskell: https://wiki.haskell.org/Working_with_time
- Documentação da biblioteca `old-time`: http://hackage.haskell.org/package/old-time-1.1.0.3/docs/Old-Time.html