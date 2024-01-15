---
title:                "Obtendo a data atual"
html_title:           "Elm: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Se você está trabalhando em um projeto que requer funcionalidade de data, é essencial saber como obter a data atual. Isso permite que você crie recursos dinâmicos e interativos para seus usuários.

## Como Fazer

Obter a data atual em Elm é simples e pode ser feito em apenas algumas linhas de código. Primeiro, importe o módulo Date no topo do seu arquivo.

```
import Date exposing (today)
```

Em seguida, use a função `today` para obter a data atual.

```
currentDate : Date
currentDate = today
```

Se quiser exibir a data em um formato específico, você pode usar a função `toTime` com um objeto de opções para especificar o formato desejado.

```
import Date.Format exposing (TimeFormat, format)
import Date exposing (today)

currentDate : String
currentDate = today
    |> toTime options
    |> format

options : TimeFormat
options =
    TimeFormat.default
        |> TimeFormat.custom "dd/MM/yyyy"
```

Isso irá retornar a data no formato especificado, como por exemplo `12/07/2021`. Você também pode substituir `today` por uma data específica para obter informações sobre aquela data em particular.

## Aprofundar-se

Elm usa um formato de data chamado de Time.Posix, que representa o número de segundos desde 1 de janeiro de 1970. Isso é semelhante ao formato usado em outras linguagens de programação, como Java e JavaScript.

Se você precisar converter a data para outro formato, como ISO 8601 ou Unix Epoch, Elm oferece funções para isso. Você também pode realizar operações com datas, como adicionar ou subtrair dias, semanas ou meses.

## Veja Também

- [Documentação do módulo Date em Elm](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Tutorial de Elm para iniciantes](https://braziljs.org/blog/uma-introducao-ao-elm-para-iniciantes/)