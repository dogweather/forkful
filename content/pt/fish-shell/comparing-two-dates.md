---
title:    "Fish Shell: Comparando duas datas"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que comparar duas datas em Fish Shell?

Comparar datas é uma tarefa comum na programação, especialmente em projetos que envolvem agendamentos, prazos e outras informações baseadas em tempo. Com o Fish Shell, é possível comparar duas datas de forma simples e eficiente através de alguns comandos específicos. Neste artigo, mostraremos como fazer isso e também discutiremos alguns detalhes mais aprofundados sobre o tema.

## Como comparar duas datas em Fish Shell

Para comparar duas datas em Fish Shell, você precisará usar o comando `date` seguido da opção `-j` para garantir que a data seja convertida para um formato numérico. Então, você pode usar o operador de comparação `<` ou `>` para determinar se a primeira data é anterior ou posterior à segunda data. Por exemplo:

```Fish Shell
date -j +%Y%m%d 20210310 < date -j +%Y%m%d 20210311
```

Este comando irá comparar a data 10/03/2021 com a data 11/03/2021 e irá retornar `true` pois a primeira data é anterior à segunda. Você também pode usar esses comandos em conjunto com as estruturas de controle `if` e `else` para realizar ações condicionais de acordo com o resultado da comparação.

## Mais detalhes sobre comparar duas datas

Ao comparar duas datas em Fish Shell, é importante levar em consideração o formato em que elas estão sendo representadas. O comando `date` permite que você escolha diferentes formatos para exibir a data, mas você precisa garantir que os formatos sejam consistentes ao fazer a comparação. Por exemplo, se você usar o formato `%d%m%Y`, o resultado da comparação será baseado no dia, mês e ano, enquanto usando `%Y%m%d`, será baseado no ano, mês e dia.

Além disso, é importante lembrar que o operador de comparação `>` também inclui igualdade. Portanto, se você quiser excluir a possibilidade de igualdade e comparar apenas se a primeira data é estritamente maior do que a segunda, você pode usar o operador `>` seguido do operador `&&` e do operador `!=`. Por exemplo:

```Fish Shell
date -j +%Y%m%d 20210311 > date -j +%Y%m%d 20210310 && date -j +%Y%m%d 20210311 != date -j +%Y%m%d 20210310
```

Este comando irá comparar as mesmas datas do exemplo anterior, mas desta vez excluindo a possibilidade de igualdade e retornará `true` apenas se a primeira data for estritamente maior do que a segunda.

## Veja também

- [Documentação oficial do Fish Shell sobre o comando 'date'](https://fishshell.com/docs/current/cmds/date.html)
- [Guia de referência rápida para o operador de comparação '&&'](https://fishshell.com/docs/current/cmds/and.html)
- [Artigo sobre formatação de datas em Fish Shell (em inglês)](https://www.networkworld.com/article/3270434/quick-date-manipulation-in-fish-shell.html)