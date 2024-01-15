---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "Go: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Quem nunca precisou saber uma data no futuro ou no passado? Talvez você precise calcular uma data de entrega, um prazo para completar uma tarefa ou simplesmente saber qual dia da semana cai no seu aniversário no próximo ano. Com Go, podemos facilmente calcular datas no futuro ou no passado usando a biblioteca de tempo padrão.

## Como fazer

Primeiro, vamos importar a biblioteca de tempo com a seguinte linha de código:

```Go
import "time"
```

Para calcular uma data no futuro, vamos usar a função `AddDate()` que aceita três parâmetros: anos, meses e dias. Vamos supor que queremos saber a data daqui a um ano:

```Go
future := time.Now().AddDate(1, 0, 0)
```

Isso irá retornar um valor do tipo `Time` com a data calculada. Agora, se quisermos saber a data no passado, podemos usar valores negativos para os parâmetros. Por exemplo, para saber a data de um ano atrás, podemos usar:

```Go
past := time.Now().AddDate(-1, 0, 0)
```

Para imprimir o resultado, podemos usar o formato de data `02-Jan-2006`, que é a data usada no layout padrão para formatação de datas. Por exemplo:

```Go
futureString := future.Format("02-Jan-2006")
fmt.Println("A data no futuro é:", futureString)

pastString := past.Format("02-Jan-2006")
fmt.Println("A data no passado é:", pastString)
```

A saída será:

```
A data no futuro é: 26-Jul-2022
A data no passado é: 26-Jul-2020
```

## Mergulho Profundo

Além da função `AddDate()`, a biblioteca de tempo também possui outras funções úteis para calcular datas, como `Add()`, `Sub()` e `Date()`. Você pode conferir a documentação completa da biblioteca em [golang.org/pkg/time](https://golang.org/pkg/time/).

## Veja também

- [Manipulando Datas no Go](https://www.callicoder.com/golang-manipulating-dates/)
- [Documentação da biblioteca de tempo](https://golang.org/pkg/time/)