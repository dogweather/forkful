---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Conversão de Data para String em Haskell

## O quê e Por quê?

Converter uma data para uma string é transformar uma estrutura de data num formato legível como texto. Os programadores fazem isso para exibir datas de maneira amigável ou para salvar datas num arquivo.

## Como Fazer

Em Haskell, você pode usar a biblioteca "Data.Time.Format" para converter uma data para uma string formatada. Veja o código:

```Haskell
import Data.Time
import Data.Time.Format

formatarData :: UTCTime -> String
formatarData data = formatTime defaultTimeLocale "%d/%m/%Y" data
```

E para usar essa função:

```Haskell
import Data.Time

main = do
    tempoAtual <- getCurrentTime
    let strData = formatarData tempoAtual
    putStrLn strData 
```

A saída será algo como "16/03/2021".

## Mergulhando Mais Fundo 

A função `formatTime` desde a versão 4.7 do pacote "time" foi como lidamos com essa conversão. Isso baseia-se na forma como as funções de formatação de tempo são implementadas no C.

Como alternativa, o pacote "time-format" não faz parte da biblioteca padrão, mas pode fornecer mais flexibilidade se formatos complexos forem necessários.

Os detalhes da implementação de `formatTime` envolvem uso de casamento de padrões em caracteres de formato específicos, e então substituindo-os com partes correspondentes da data.

## Veja Também

  * A [documentação do pacote "time"](https://hackage.haskell.org/package/time) é super útil para aprender sobre as diversas maneiras que Haskell lida com datas e tempos.
  * Este [tutorial do "Time and Dates in Haskell"](https://two-wrongs.com/time-and-dates-in-haskell) também pode ser útil para entender esses conceitos.