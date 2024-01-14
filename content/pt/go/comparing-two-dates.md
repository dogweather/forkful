---
title:                "Go: Comparando duas datas"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas no Go?

Comparar datas é uma tarefa comum em muitos projetos de programação. No Go, podemos usar diferentes técnicas para comparar duas datas e determinar se uma é anterior, posterior ou igual à outra. Neste artigo, discutiremos por que é importante aprender a comparar datas no Go e como fazê-lo de maneira eficiente.

## Como Comparar Duas Datas no Go

Para comparar duas datas no Go, podemos usar o método `Before`, `After` ou `Equal` da biblioteca `time` do Go. Vamos dar uma olhada em alguns exemplos de código para ver como isso funciona:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    data1 := time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC)
    data2 := time.Date(2020, 1, 1, 0, 0, 0, 0, time.UTC)

    fmt.Println(data1.Before(data2)) // Output: false
    fmt.Println(data1.After(data2))  // Output: true
    fmt.Println(data1.Equal(data2))  // Output: false
}
```

Neste exemplo, criamos duas variáveis `data1` e `data2` contendo diferentes datas. Em seguida, utilizamos os métodos `Before`, `After` e `Equal` para compará-las. O método `Before` retorna `true` se a primeira data for anterior à segunda, `After` retorna `true` se a primeira data for posterior à segunda e `Equal` retorna `true` se as duas datas forem iguais.

É importante notar que esses métodos comparam as datas em relação à sua referência de fuso horário. Portanto, ao criar as variáveis `data1` e `data2`, definimos seu fuso horário como UTC para garantir uma comparação precisa.

## Deep Dive: Comparando Datas com Mais Precisão

Além dos métodos `Before`, `After` e `Equal`, a biblioteca `time` do Go também possui um método `BeforeTime` que permite comparar datas com mais precisão, levando em consideração não apenas o ano, mês e dia, mas também a hora, minuto e segundo. Podemos usar esse método da seguinte maneira:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    data1 := time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC)
    data2 := time.Date(2021, 1, 1, 0, 0, 1, 0, time.UTC)

    fmt.Println(data1.BeforeTime(data2)) // Output: true
}
```

Neste exemplo, definimos a variável `data2` com uma diferença de 1 segundo em relação à variável `data1`. Ao usar o método `BeforeTime`, o resultado é `true` porque a variável `data1` é anterior à `data2` em termos de hora e minuto, embora o dia, mês e ano sejam iguais.

## Veja Também

- Documentação oficial do pacote `time` no Go: https://golang.org/pkg/time/
- Tutorial sobre como trabalhar com datas e horários no Go: https://golangbyexample.com/dates-times-go-tutorial/
- Guia completo do Go: https://www.golang-book.com/books/intro