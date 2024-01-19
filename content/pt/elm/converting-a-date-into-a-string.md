---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Converter uma data para uma string significa mudar uma instancia de data para a representação textual. Programadores fazem isso para melhores exibições na interface do usuário, para registro, ou para operações de comparação mais simples.

## Como fazer:

Aqui está como você pode converter uma data para string no Elm:

```Elm
import Time exposing (Posix, toIsoString)

dateToString : Posix -> String
dateToString date =
    Time.toIsoString date
```

Quando você executa o código acima com uma data, irá retornar a representação em string daquela data. Por exemplo:

```Elm
dateToString (Time.millisToPosix 1561653177216) 
```

Isto irá retornar `2019-06-27T17:19:37.216Z`.

## Mais a Fundo:

Historicamente, conversões de data para string são uma prática comum na programação, não restrita apenas ao Elm. Essa funcionalidade é especialmente útil quando trabalha-se com APIs e armazenamento de dados, onde as datas frequentemente precisam ser convertidas em strings, e vice versa.

Porém, há alternativas ao `toIsoString`. Por exemplo, você pode usar o `toString` para conseguir uma string mais fácil de ler. No Elm, o módulo `Time` possui várias utilidades para trabalhar com datas e horas.

Na implementação, a função `toIsoString` converte um `Posix` (únix timestamp) para uma string formatada de acordo com o ISO 8601, um padrão de data internacional. Isto é útil para manter uma consistência nos múltiplos formatos de data em todo o mundo.

## Veja Também:

Para mais informações sobre programação em Elm e manipulação de data e hora, dê uma olhada nesses links:

- A documentação oficial do Elm: [https://elm-lang.org/docs](https://elm-lang.org/docs)
- Um guia sobre `Time.Posix` no Elm: [https://package.elm-lang.org/packages/elm/time/latest/Time-Posix](https://package.elm-lang.org/packages/elm/time/latest/Time-Posix) 
- Informações sobre a norma ISO 8601: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)