---
title:                "Elm: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Porque

Cálculos de datas no futuro e passado são uma parte importante da programação em Elm. Eles permitem que os desenvolvedores trabalhem com diferentes cenários de tempo em seus aplicativos, como agendamento de tarefas ou exibição de eventos futuros.

##Como Fazer

Para calcular uma data no futuro ou passado em Elm, usamos a biblioteca `elm/time` que fornece funções úteis para manipulação de datas e horas. Para começar, importamos a biblioteca em nosso código:

```Elm
import Time exposing (..)
```

Agora, podemos usar as funções fornecidas pela biblioteca para realizar nossos cálculos. Por exemplo, para obter a data atual, usamos a função `now` e para adicionar um certo número de segundos a essa data, usamos a função `add`:

```Elm
-- obtenha a data atual
now

-- adicione 5 segundos à data atual
add (seconds 5) now
```

O resultado dessas operações será do tipo `Time Posix` que representa um momento específico no tempo. Podemos então formatá-lo usando a função `toString` para exibir a data de forma mais legível:

```Elm
-- obtenha a data atual em formato de string
toString now
```

Podemos até mesmo passar um formato específico como argumento para a função `toString`, como por exemplo:

```Elm
-- obtenha a data atual no formato "dia/mês/ano"
toString (customFormat "%d/%m/%Y" now)
-- output: "18/12/2020"
```

##Mergulho Profundo

Ao lidar com datas e horas em Elm, é importante entender que a linguagem usa o tipo `Time Posix` para representar um momento específico no tempo. Isso é diferente de outras linguagens que usam um tipo de dados mais genérico, o que pode criar certa confusão.

Além disso, a biblioteca `elm/time` também oferece funções como `daysInMonth` e `addGregorianYears`, que podem ser úteis ao trabalhar com cálculos de datas mais complexos.

##Veja Também

- Documentação oficial da biblioteca `elm/time`: https://package.elm-lang.org/packages/elm/time/latest/
- Tutorial de Elm sobre manipulação de datas e horas: https://www.elm-tutorial.org/en/09-elm-time.html