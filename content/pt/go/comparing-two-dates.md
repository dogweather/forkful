---
title:    "Go: Comparando duas datas"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que comparar duas datas em programas Go?
Muitas vezes, ao lidar com datas em programas Go, pode ser necessário comparar duas datas para verificar se uma é anterior, posterior ou igual à outra. Isso é especialmente útil em casos em que é preciso verificar a validade de uma entrada de data ou para ordenar uma lista de datas. Neste artigo, vamos mostrar como comparar duas datas em programas Go e explicar por que isso é importante.

## Como Comparar Duas Datas em Programas Go
Para comparar duas datas em programas Go, podemos utilizar o método `Before()` ou `After()` da struct `time.Time`. Esses métodos retornam um valor booleano indicando se a primeira data é antes ou depois da segunda data, respectivamente. Veja o exemplo abaixo:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    data1 := time.Date(2020, time.June, 16, 0, 0, 0, 0, time.UTC)
    data2 := time.Date(2020, time.June, 18, 0, 0, 0, 0, time.UTC)

    if data1.Before(data2) {
        fmt.Println("data1 é anterior a data2")
    }

    if data2.After(data1) {
        fmt.Println("data2 é posterior a data1")
    }
}
```

A saída desse código será:

```
data1 é anterior a data2
data2 é posterior a data1
```

Além desses métodos, também é possível utilizar o operador `<` ou `>` para comparar datas em programas Go. Veja o exemplo abaixo:

```Go
if data1 < data2 {
    fmt.Println("data1 é anterior a data2")
}

if data2 > data1 {
    fmt.Println("data2 é posterior a data1")
}
```

Essa abordagem também retorna um valor booleano indicando a comparação entre as duas datas.

## Profundando na Comparação de Datas em Programas Go
Em Go, ao comparar duas datas, não apenas é levado em consideração o valor numérico de cada data, mas também o seu fuso horário. Isso significa que duas datas com o mesmo valor numérico, mas em fusos horários diferentes, podem ser consideradas diferentes em uma comparação de datas. Por exemplo, `03/05/2020 00:00:00` em Brasília seria igual a `02/05/2020 23:00:00` em Nova York.

Outra coisa importante a considerar é que a comparação de datas também pode ser feita entre `nil`. Nesse caso, o valor booleano retornado será `false`, pois uma data `nil` não pode ser comparada com nenhuma outra data.

## Veja também
- [Documentação do Pacote Time em Go](https://golang.org/pkg/time/)
- [Tutorial: Manipulação de Datas em Go](https://golangbot.com/date-time/)
- [Como Trabalhar com Datas em Go](https://www.calhoun.io/working-with-dates-in-golang/)