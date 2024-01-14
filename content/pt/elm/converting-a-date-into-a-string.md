---
title:    "Elm: Convertendo uma data em uma string"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Existem muitas situações em que precisamos converter uma data em uma string - seja para exibição na interface do usuário ou para manipulação de dados em um sistema. É uma tarefa bastante comum na programação e, neste artigo, vamos explorar como podemos realizar essa conversão em Elm.

## Como fazer

Para converter uma data em uma string em Elm, precisamos utilizar a função `toString` do pacote `Date`. Ela aceita uma data no formato `Date` e retorna uma string no formato `YYYY-MM-DD`.

```
Elm pacote de data
data = Date.fromCalendarDate 2022 1 5
string = Date.toString data
```

O exemplo acima produzirá a seguinte string: `2022-01-05`.

Também podemos especificar um formato personalizado para a string, utilizando a função `format` do pacote `Date`. Ela aceita um formato desejado, juntamente com a data, e retorna uma string com a data formatada de acordo com o padrão especificado.

```
Elm pacote de data
data = Date.fromCalendarDate 2022 1 5
string = Date.format "%d/%m/%Y" data
```

Este exemplo produzirá a seguinte string: `05/01/2022`.

## Navegação profunda

Ao converter uma data em uma string, também podemos realizar outras operações com a função `toDate` do pacote `Date`. Ela aceita uma string e um formato e retorna uma data no formato `Date`.

```
Elm pacote de data
string = "2022-01-05"
data = Date.fromString "%Y-%m-%d" string
```

Este exemplo produzirá a data `05/01/2022` no formato `Date`, que então pode ser utilizada para realizar operações como calcular o dia da semana ou compará-la com outras datas.

## Veja também

- Documentação do pacote `Date`: https://package.elm-lang.org/packages/elm/time/latest/
- Tutorial de programação Elm: https://guide.elm-lang.org/
- Discussões sobre Elm na comunidade brasileira: https://groups.google.com/g/elmbr