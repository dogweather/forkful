---
title:                "Elm: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que usar Elm para obter a data atual?

Se você está familiarizado com programação, sabe como pode ser frustrante trabalhar com datas e horários. Além disso, dependendo da linguagem de programação que você está usando, pode haver diferenças na formatação e na precisão da data. No entanto, com Elm, é muito mais fácil obter a data atual e manipulá-la da maneira desejada. Então, se você está cansado de lutar com datas em outras linguagens, pode ser hora de considerar o uso de Elm.

## Como fazer para obter a data atual em Elm?

Para começar, você precisa importar o módulo `Time` no início do seu código. Em seguida, você pode usar a função `now` do módulo para obter a data e hora atuais. Aqui está um exemplo de código que retorna a data atual formatada como dia/mês/ano:

``` Elm
import Time

atualizarData : String
atualizarData =
  Time.now
    |> Time.toDate
    |> Time.format "%d/%m/%Y"
```

Ao executar este código, o valor retornado será algo como `09/09/2021`.

## Mergulho mais profundo

Em Elm, a data é representada pelo tipo `Date`, que contém informações sobre ano, mês e dia. Além disso, o módulo `Time` possui muitas outras funções úteis para trabalhar com datas, como `dayOf` (para obter o dia do mês), `monthOf` (para obter o mês) e `yearOf` (para obter o ano). Você também pode usar essas funções para criar datas personalizadas e compará-las.

Outra coisa interessante é que o módulo `Time` também possui funções para trabalhar com horários e fusos horários, o que pode ser útil se você estiver criando uma aplicação em várias regiões do mundo.

## Veja também

- Documentação do módulo `Time`: https://package.elm-lang.org/packages/elm/time/latest/
- Exemplos práticos de uso do módulo `Time`: https://elmprogramming.com/working-with-dates-times-elm.html#Dates-and-Time-in-Elm

Agora que você sabe como obter a data atual em Elm, experimente aplicá-la em seu próximo projeto e veja como pode facilitar o trabalho com datas e horários. Com o módulo `Time`, não é mais necessário se preocupar com as diferenças de formatação e precisão da data em diferentes linguagens de programação. Continue aprendendo e explorando os recursos do Elm para aprimorar ainda mais suas habilidades de programação.