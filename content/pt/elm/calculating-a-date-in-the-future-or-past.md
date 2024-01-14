---
title:    "Elm: Calculando uma data no futuro ou passado"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por que calcular uma data futura ou passada em Elm?

Calcular datas futuras ou passadas pode ser útil em uma variedade de situações de programação, como a criação de calendários ou agendas, agendamento de eventos e outras aplicações relacionadas ao tempo. Com o Elm, é possível realizar esses cálculos com facilidade, graças às suas funções de manipulação de datas.

# Como fazer em Elm

Para calcular uma data futura ou passada em Elm, primeiro precisamos importar o módulo Date e a função addDays. Dessa forma, podemos adicionar um determinado número de dias a uma data específica e obter uma nova data resultante. Por exemplo, para calcular uma data 30 dias no futuro, podemos fazer o seguinte:

```Elm
import Date exposing (..)
import Date.Extra exposing (addDays)

dataAtual = Date.fromIsoString "2021-07-10"
dataFutura = addDays 30 dataAtual
```

Neste exemplo, definimos uma data atual e, em seguida, adicionamos 30 dias a ela usando a função addDays. Isso resulta na data "2021-08-09".

Também podemos usar o mesmo método para calcular uma data passada, simplesmente alterando o sinal do número de dias para um valor negativo. Por exemplo, para calcular uma data 30 dias no passado, podemos fazer o seguinte:

```Elm
import Date exposing (..)
import Date.Extra exposing (addDays)

dataAtual = Date.fromIsoString "2021-07-10"
dataPassada = addDays -30 dataAtual
```

Isso resultará na data "2021-06-10".

# Mais detalhes sobre cálculos de data em Elm

Além de adicionar dias a uma data, existem outras funções úteis no módulo Date para manipular datas em Elm. Algumas delas incluem:

- `addMonths`: adiciona um determinado número de meses a uma data
- `addYears`: adiciona um determinado número de anos a uma data
- `subtract`: subtrai uma outra data de uma data específica

É importante notar que todas essas funções retornam um novo valor de data, em vez de alterar o valor original. Isso garante uma programação mais segura e evita efeitos colaterais indesejados.

Além disso, o módulo Date também possui funções para obter informações sobre uma data, como o dia da semana, o último dia do mês e assim por diante. Consulte a documentação oficial do Elm para obter mais detalhes sobre essas funções.

# Veja também
- Documentação oficial do módulo Date em Elm: https://package.elm-lang.org/packages/elm-lang/core/latest/Date
- Exemplos de cálculos de data em Elm: https://ellie-app.com/new
- Tutorial sobre manipulação de data em Elm: https://programmingwithmosh.com/elm/elm-dates/