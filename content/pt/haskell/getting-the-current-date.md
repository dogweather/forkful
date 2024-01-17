---
title:                "Obtendo a data atual."
html_title:           "Haskell: Obtendo a data atual."
simple_title:         "Obtendo a data atual."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Obter a data atual significa receber a data atual do sistema em que o programa está sendo executado. Isso é útil para tarefas como registro de logs ou criação de arquivos com marcações de data. Programadores usam isso para garantir que o programa tenha acesso à data correta e atual.

## Como fazer:

Usando o módulo 'Data.Time', basta importar o módulo e chamar a função 'getCurrentTime' para obter a data atual. Em seguida, você pode usar funções do módulo para formatar a data como desejar.

```Haskell
import Data.Time

main = do
    currentTime <- getCurrentTime
    print currentTime
```

Isso irá imprimir a data e hora com fuso horário:

```
2021-09-19 15:30:46.123456 UTC
```

## Profundidade

Este módulo foi adicionado pela primeira vez ao Haskell em 2002 e foi inspirado pelo módulo 'time' em C. Existem outras maneiras de obter a data atual, como usando a função 'time' do módulo 'System.Posix'.

Ao usar a função 'getCurrentTime', o valor retornado é do tipo 'UTCTime', que representa um instante absoluto no tempo. Este valor pode ser convertido para o fuso horário local usando a função 'utcToLocalZonedTime'.

## Veja também

- [Documentação do módulo 'Data.Time'](https://hackage.haskell.org/package/time-1.10.0.0/docs/Data-Time.html)
- [Tutorial sobre 'Data.Time'](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples#time)
- [Tutorial sobre 'System.Posix'](https://www.schoolofhaskell.com/user/Gelopfalcon/systemposix)