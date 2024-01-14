---
title:    "Elm: Obtendo a data atual."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que

Neste post, vamos falar sobre como obter a data atual em um programa escrito em Elm. Isso pode ser útil para vários fins, como exibir a data em um aplicativo ou fazer cálculos baseados na data atual.

## Como fazer

Para obter a data atual em Elm, podemos usar a função `Date.now`, que retorna o número de milissegundos desde 1º de janeiro de 1970. Podemos então usar essa informação para criar uma `Date` da Elm. Aqui está um exemplo de código:

```Elm
import Date exposing (Date)

date : Date
date = Date.now |> Date.fromMilliseconds
```

Neste exemplo, estamos importando o módulo `Date` e usando a função `fromMilliseconds` para criar um valor `Date` a partir do número de milissegundos obtido com `Date.now`. Podemos então usar essa variável para exibir a data em nosso aplicativo.

```Elm
view : Date -> Html msg
view date =
    div [] [ text <| Date.toString Date.full date ]

main =
    Html.program
        { init = date
        , view = view
        , update = always date
        }
```

O resultado deste código será a data atual formatada em sua localização. Por exemplo, se estivermos em Portugal, a data será exibida no formato "Dia da semana, dia de mês de ano". Você pode experimentar este código e ver a data sendo exibida atualizada a cada segundo.

## Profundidade

Além de `Date.now` e `Date.fromMilliseconds`, o módulo `Date` possui muitas outras funções úteis para trabalhar com datas, como `Date.fromString` para converter uma string em uma data e `Date.compare` para comparar duas datas. Recomendamos que você dê uma olhada na documentação do módulo `Date` para descobrir todas as opções disponíveis.

## Veja também

- [Documentação do módulo Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Exemplos de código Elm](https://elm-lang.org/examples)

Aqui terminamos nosso post sobre como obter a data atual em Elm. Esperamos que tenha sido útil e que você possa aplicar esse conhecimento em seus próprios projetos. Não se esqueça de dar uma olhada nos links fornecidos para mais recursos sobre programação em Elm. Até a próxima!