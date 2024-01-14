---
title:                "Go: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Hoje vamos falar sobre a conversão de uma data em uma string no Go. Esta é uma tarefa comum em muitos projetos de programação e entender esse processo pode ajudar a melhorar suas habilidades de programação em Go.

## Como Fazer

Vamos começar com uma data básica no formato `date.Month Dia, Ano` e mostrar como podemos convertê-la em uma string. Usando a função `fmt.Sprintf`, podemos formatar a data para atingir o nosso objetivo:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    data := time.Date(2020, time.September, 30, 0, 0, 0, 0, time.UTC)
    dataStr := fmt.Sprintf("%s %d, %d", data.Month(), data.Day(), data.Year())
    fmt.Println(dataStr)
}
```

Se rodarmos esse código, a saída será `setembro 30, 2020`, que é exatamente o que queríamos. A função `fmt.Sprintf` nos permite usar verbos para formatar os valores passados, como por exemplo `%s` para uma string, `%d` para um inteiro e `%v` para um valor qualquer.

## Mergulho Profundo

Em Go, existem diferentes maneiras de formatar uma data para string, cada uma com suas próprias vantagens e desvantagens. Se quisermos incluir informações como o dia da semana ou o fuso horário na string, podemos usar a função `Format` do pacote `time`.

Além disso, devemos ter em mente que a conversão de uma data em uma string também depende do formato do idioma em que estamos trabalhando. O pacote `time` inclui recursos para fazer isso de forma fácil e eficiente.

Para mais informações sobre como formatar datas em Go, recomendamos conferir a documentação oficial do pacote `time` e também explorar alguns exemplos na internet.

## Veja Também

- [Documentação do Pacote Time em Go](https://pkg.go.dev/time)
- [Exemplo de Formatação de Data em Go](https://www.w3schools.in/go/date-time-format/)
- [Tutorial de Formatação de Data em Go](https://golangbyexample.com/go-date-format/)