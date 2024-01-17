---
title:                "Comparando duas datas"
html_title:           "Go: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que e por que?

Comparar duas datas no desenvolvimento de software é uma tarefa comum, pois permite verificar se uma data é antes, depois ou igual a outra. Isso é importante para diversas aplicações, como filtrar dados de acordo com a data, validar entradas do usuário ou realizar cálculos baseados nas datas.

## Como fazer:

A linguagem Go possui uma função nativa para comparar datas chamada `Equal`. Ela recebe dois parâmetros do tipo `time.Time` e retorna um valor booleano indicando se as datas são iguais ou não. Veja um exemplo abaixo:

```
data1 := time.Date(2020, time.December, 12, 0, 0, 0, 0, time.UTC)
data2 := time.Date(2020, time.December, 12, 0, 0, 0, 0, time.UTC)

resultado := time.Equal(data1, data2)

fmt.Println("As datas são iguais?", resultado)
// Output: As datas são iguais? true
```

Além da função `Equal`, também é possível utilizar os operadores de comparação `==` (igual), `>` (maior), `<` (menor), `>=` (maior ou igual) e `<=` (menor ou igual) para comparar datas em Go.

## Mais detalhes:

Comparar datas é uma tarefa que existe desde os primórdios da computação, pois a manipulação de datas é fundamental para a utilização de sistemas de informação. Além da função `Equal`, existem outras bibliotecas em Go que oferecem funcionalidades para manipulação de datas, como `time.Parse` para converter uma string em uma data e `time.Format` para formatar uma data em uma string.

## Veja também:

- Documentação oficial da função `Equal` em Go: https://golang.org/pkg/time/#Equal
- Comparação entre datas em outras linguagens de programação: https://www.oreilly.com/library/view/data-algorithms/9781491906170/ch02.html
- Manipulação de datas em Go: https://www.calhoun.io/parsing-dates-and-times-in-go/